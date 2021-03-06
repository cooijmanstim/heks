(in-package :heks)

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
              :accessor minimax-agent-evaluator)
   transposition-table
   killer-table))

(defmethod initialize ((agent minimax-agent) state)
  (with-slots (evaluator transposition-table killer-table) agent
    (evaluation* evaluator state)
    (setf transposition-table (make-transposition-table))
    (setf killer-table (make-killer-table))))

(defmethod update ((agent minimax-agent) state move breadcrumb)
  (with-slots (evaluator killer-table) agent
    (evaluation+ evaluator state move breadcrumb)
    (update-killer-table killer-table)))

(defmethod downdate ((agent minimax-agent) state move breadcrumb)
  (with-slots (evaluator killer-table) agent
    (evaluation- (minimax-agent-evaluator agent) state move breadcrumb)
    (downdate-killer-table killer-table)))

(defmethod decide ((agent minimax-agent) state &key (time 10))
  (time-limited time
                (lambda ()
                  (with-slots (evaluator transposition-table killer-table) agent
                    (let ((*transposition-table* transposition-table)
                          (*killer-table* killer-table))
                      (minimax-decision state evaluator))))))

(defmethod cleanup ((agent minimax-agent))
  ;; clean up ye olde table
  (with-slots (transposition-table) agent
    (setf transposition-table nil))
  (sb-ext:gc :full t))

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

(defparameter *ponder-thread-count* 2)
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
  (with-slots (ponderers keep-pondering evaluator) agent
    (setf keep-pondering nil)
    (dolist (ponderer ponderers)
      (sb-thread:join-thread ponderer :default nil))
    (when-let ((node (pmcts-tree-root-node (slot-value evaluator 'tree))))
      (mcts-node-destroy node))))

(defclass time-managing-agent (agent)
  ((agent :initarg :agent)
   (planner :initarg :planner)))

(defmethod initialize ((agent time-managing-agent) state)
  (with-slots (agent planner) agent
    (initialize agent state)
    (initialize-planner planner state)))

(defmethod decide ((agent time-managing-agent) state &key time)
  (with-slots (agent planner) agent
    (labels ((decision-fn (time)
               (decide agent state :time time)))
      (if time
          (decision-fn time)
          (plan-and-decide planner state #'decision-fn)))))

(defmethod update ((agent time-managing-agent) state move breadcrumb)
  (update (slot-value agent 'agent) state move breadcrumb))

(defmethod downdate ((agent time-managing-agent) state move breadcrumb)
  (downdate (slot-value agent 'agent) state move breadcrumb))

(defmethod cleanup ((agent time-managing-agent))
  (cleanup (slot-value agent 'agent)))
