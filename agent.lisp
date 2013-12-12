(in-package :heks)

;; TODO: time

(defclass agent () ())

(defgeneric initialize (agent state)
  (:method ((agent agent) state)))
(defgeneric update   (agent state move breadcrumb)
  (:method ((agent agent) state move breadcrumb)))
(defgeneric downdate (agent state move breadcrumb)
  (:method ((agent agent) state move breadcrumb)))
(defgeneric decide (agent state &key time))
(defgeneric cleanup (agent)
  (:method ((agent agent))))

(defclass minimax-agent (agent)
  ((evaluator :type evaluator
              :initarg :evaluator
              :accessor minimax-agent-evaluator)))

(defmethod initialize ((agent minimax-agent) state)
  (evaluation* (minimax-agent-evaluator agent) state))

(defmethod update ((agent minimax-agent) state move breadcrumb)
  (evaluation+ (minimax-agent-evaluator agent) state move breadcrumb))

(defmethod downdate ((agent minimax-agent) state move breadcrumb)
  (evaluation- (minimax-agent-evaluator agent) state move breadcrumb))

(defmethod decide ((agent minimax-agent) state &key (time 10))
  ;; TODO: use an instance variable instead of global *out-of-time*
  ;; TODO: judge amount of time needed
  (time-limited time
                (lambda ()
                  (with-slots (evaluator) agent
                    (minimax-decision state evaluator)))))

(defclass pmcts-agent (agent)
  ((tree :type pmcts-tree
         :initarg :tree
         :accessor pmcts-agent-tree)))

(defmethod initialize ((agent pmcts-agent) state)
  (setf (slot-value agent 'tree) (make-pmcts-tree-for-state state)))

(defmethod update   ((agent pmcts-agent) state move breadcrumb)
  (update-pmcts-tree (pmcts-agent-tree agent) state move breadcrumb))

(defmethod downdate ((agent pmcts-agent) state move breadcrumb)
  (downdate-pmcts-tree (pmcts-agent-tree agent) state move breadcrumb))

(defmethod decide ((agent pmcts-agent) state &key (time 10))
  (time-limited time
                (lambda ()
                  (mcts-decision state :root-node (pmcts-tree-current-node (pmcts-agent-tree agent))))))

(defparameter *ponder-thread-count* 1)
;; for when we need to stop the ponderers gracefully
(defparameter *keep-pondering* t)

(defun minimax-pmcts-ponder (agent)
  (iter (while (and (slot-value agent 'keep-pondering) *keep-pondering*))
        (let (node state)
          (with-slots (current-node current-state current-lock) agent
            (sb-thread:with-mutex (current-lock)
              (setf node current-node
                    state current-state)))
          (mcts-sample node state))))

(defun minimax-pmcts-refocus (agent state node)
  (with-slots (current-state current-node current-lock) agent
    (sb-thread:with-mutex (current-lock)
      (setf current-state state
            current-node node))))

(defclass minimax-pmcts-agent (minimax-agent)
  ((evaluator :initform (make-instance 'pmcts-evaluator))
   ;; list of pondering threads
   (ponderers :initform '())
   ;; to signal they should stop
   (keep-pondering :initform t)
   ;; what they are pondering (current-node shares structure with evaluator's tree)
   (current-node :initform nil)
   (current-state :initform nil)
   ;; lock held when changing current-node/current-state
   (current-lock :initform (sb-thread:make-mutex :name "pondering-current-lock"))))

(defmethod initialize :after ((agent minimax-pmcts-agent) state)
  (with-slots (evaluator ponderers) agent
    (let* ((state (copy-state state))
           (node (slot-value (slot-value evaluator 'tree) 'current-node)))
      (minimax-pmcts-refocus agent state node))
    (dotimes (i *ponder-thread-count*)
      (far-push (sb-thread:make-thread
                 #'minimax-pmcts-ponder :arguments (list agent)
                 :name (format nil "minimax-pmcts-ponderer ~A" i))
                ponderers))))

(defmethod update ((agent minimax-pmcts-agent) state move breadcrumb)
  (call-next-method)
  (let* ((state (copy-state state))
         (node (mcts-child-for-state (slot-value agent 'current-node) state)))
    (minimax-pmcts-refocus agent state node)
    (pmcts-evaluator-uproot (slot-value agent 'evaluator) 2)))

(defmethod downdate ((agent minimax-pmcts-agent) state move breadcrumb)
  (call-next-method)
  (let ((state (copy-state state))
        (node (mcts-node-parent (slot-value agent 'current-node))))
    (unapply-move state breadcrumb)
    (minimax-pmcts-refocus agent state node)))

(defmethod decide :after ((agent minimax-pmcts-agent) state &key (time 10))
  (declare (ignore state time))
  (with-slots (evaluator) agent
    (fresh-line)
    (format t "mean pmcts evaluator support: ~A" (slot-value evaluator 'support-mean))))

(defmethod cleanup :after ((agent minimax-pmcts-agent))
  (with-slots (ponderers keep-pondering) agent
    (setf keep-pondering nil)
    (dolist (ponderer ponderers)
      (sb-thread:join-thread ponderer :default nil))))

(defclass time-managing-agent (agent)
  ((agent :initarg :agent)
   (time-left :type single-float :initarg :total-time :initform most-positive-single-float)
   (variance-mean :initform 0.0 :type single-float)
   (variance-sample-size :initform 0 :type integer)
   (moves-left-mean :initform 0.0 :type single-float)
   (moves-left-sample-size :initform 0 :type integer)))

(defparameter *tb-ma-sample-budget* 1000)
(defparameter *tb-ma-sample-depth* 3)
(defparameter *tb-ml-sample-budget* 100)

(defun sample-material-advantages (state)
  (let ((evaluator (make-instance 'material-evaluator))
        (sample (make-array *tb-ma-sample-budget* :fill-pointer 0))
        (state (copy-state state)))
    (evaluation* evaluator state)
    (iter (repeat *tb-ma-sample-budget*)
          (iter (with breadcrumbs = '())
                (with move-history = '())
                (repeat *tb-ma-sample-depth*)
                (for moves = (moves state))
                (when (state-endp state)
                  (finish))
                (let* ((move (random-elt moves))
                       (breadcrumb (apply-move state move)))
                  (push breadcrumb breadcrumbs)
                  (push move move-history)
                  (evaluation+ evaluator state move breadcrumb))
                (finally
                 (vector-push (evaluation? evaluator state (moves state)) sample)
                 (iter (for breadcrumb in breadcrumbs)
                       (for move in move-history)
                       (evaluation- evaluator state move breadcrumb)
                       (unapply-move state breadcrumb)))))
    sample))

(defun symmetric-sigmoid (x)
  (if (zerop x)
      0
      (/ x (1+ (abs x)))))

;; keep a running average of playout lengths. overestimates due to running
;; average inertia.
(defun estimate-moves-left (agent state)
  (with-slots (moves-left-mean moves-left-sample-size) agent
    (iter (repeat *tb-ml-sample-budget*)
          (for move-count = (iter (with state = (copy-state state))
                                  (for moves = (moves state))
                                  (until (emptyp moves))
                                  (count t)
                                  (apply-move state (random-elt moves))))
          (update-running-average moves-left-mean moves-left-sample-size move-count))
    moves-left-mean))

(defun time-budget (agent state time-left)
  (let* ((sample (sample-material-advantages state))
         (variance (coerce (variance sample) 'single-float)))
    (with-slots (variance-mean variance-sample-size) agent
      (update-running-average variance-mean variance-sample-size variance)
      (let* ((moves-left (estimate-moves-left agent state))
             (mean-move-time (/ time-left moves-left))
             (variance-fraction (/ variance (+ 1.0 variance-mean)))
             ;; importance ranges from 1/2 (low variance) to 2 (high variance)
             (importance (expt 2 (symmetric-sigmoid variance-fraction)))
             (time-budget (* mean-move-time importance)))
        (fresh-line)
        (print (list :time-left time-left
                     :variance variance
                     :variance-mean variance-mean
                     :moves-ahead-count moves-left
                     :mean-move-time mean-move-time
                     :variance-fraction variance-fraction
                     :importance importance
                     :time-budget time-budget))
        time-budget))))

(defmethod initialize ((agent time-managing-agent) state)
  (with-slots ((inner-agent agent) time-left) agent
    (initialize inner-agent state)
    ;; initialize mean variance estimate
    (dotimes (i 10)
      (time-budget agent state time-left))))

(defmethod decide ((agent time-managing-agent) state &key time)
  (with-slots ((inner-agent agent) time-left) agent
    (labels ((duration (t0 t1)
               (coerce (/ (- t1 t0)
                          internal-time-units-per-second)
                       'single-float)))
      (let (t0 t1 t2 decision)
        (setf t0 (get-internal-real-time))
        (unwind-protect
             (progn
               (setf time (or time (time-budget agent state time-left)))
               (setf t1 (get-internal-real-time))
               (setf decision (decide inner-agent state :time time)))
          (setf t2 (get-internal-real-time))
          (print (list :planning-time (duration t0 t1)
                       :decision-time (duration t1 t2)))
          (decf time-left (duration t0 t2)))
        decision))))

(defmethod update ((agent time-managing-agent) state move breadcrumb)
  (update (slot-value agent 'agent) state move breadcrumb))

(defmethod downdate ((agent time-managing-agent) state move breadcrumb)
  (downdate (slot-value agent 'agent) state move breadcrumb))

(defmethod cleanup ((agent time-managing-agent))
  (cleanup (slot-value agent 'agent)))
