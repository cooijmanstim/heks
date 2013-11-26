(in-package :heks)

(declaim (optimize (safety 3) (debug 3)))

(defparameter *minimax-maximum-depth* 100)

;; NOTE: we can use (state-hash state) rather than state as the hash-table key,
;; but then collisions can happen, and stored information may be invalid.  if
;; the stored move is invalid (highly likely), conditions will occur.
;; if the stored move is valid, we can still use it for move ordering.
;; TODO: measure collision rate
(defstruct transposition
  (depth 0 :type fixnum)
  (value 0 :type fixnum)
  (type nil :type (or null (member :exact :lower :upper)))
  (move nil :type list))
(sb-ext:define-hash-table-test state-equal state-hash)
(defparameter *transposition-table* nil)
;; store only deep transpositions to save memory.  shallow transpositions are used much more
;; often, but there are many more of them.  TODO: handle out of memory properly
(defparameter *transposition-minimum-depth* 2)

(defun make-transposition-table ()
  (make-hash-table :test 'state-equal))

(defun lookup-transposition (state &optional depth)
  (when-let ((transposition (gethash state *transposition-table*)))
    (if (and depth (>= depth (transposition-depth transposition)))
        nil
        transposition)))

(defun ensure-transposition (state)
  (if-let ((transposition (gethash state *transposition-table*)))
    transposition
    (setf (gethash (copy-state state) *transposition-table*)
          (make-transposition))))

(defparameter *killers* nil)

(defun make-killers ()
  (make-array (list *minimax-maximum-depth*) :initial-element '()))

(defun lookup-killers (ply valid-moves)
  (let ((killers (aref *killers* ply)))
    (intersection killers valid-moves)))

(defun store-killer (ply move)
  (deletef (aref *killers* ply) move :test #'move-equal)
  (push move (aref *killers* ply)))

(defun order-moves (depth ply state moves)
  (declare (ignore depth))
  (let ((prioritized-moves (lookup-killers ply moves)))
    ;; use transposition move regardless of depth
    (when-let ((transposition (lookup-transposition state)))
      (with-slots (move) transposition
        (push move prioritized-moves)))
    (if prioritized-moves
        (nconc prioritized-moves (nset-difference moves prioritized-moves :test #'move-equal))
        moves)))

(defstruct minimax-statistics
  (mean-branching-factor 0.0 :type single-float)
  (branching-factor-sample-size 0 :type integer)
  (mean-move-count 0.0 :type single-float)
  (move-count-sample-size 0 :type integer)
  (node-count 0 :type integer))
(defparameter *minimax-statistics* nil)

;; XXX: mean and n are places that are evaluated more than once
(defmacro update-running-average (mean n newvalue)
  `(progn (setf ,mean (+ (* (/ ,n (1+ ,n)) ,mean)
                         (* (/  1 (1+ ,n)) ,newvalue)))
          (incf ,n)))

(defun measure-branching-factor (branching-factor)
  (with-slots ((mean mean-branching-factor) (n branching-factor-sample-size)) *minimax-statistics*
    (update-running-average mean n branching-factor)))

(defun measure-moves (moves)
  (with-slots ((mean mean-move-count) (n move-count-sample-size)) *minimax-statistics*
    (update-running-average mean n (length moves))))

(defun measure-node ()
  (incf (minimax-statistics-node-count *minimax-statistics*)))

;; depth counts down as we recurse, ply counts up. transposition lookup depends on depth
;; (of the subtree it summarizes) whereas killer move lookup depends on ply.
(defun minimax (state depth ply evaluator
                &optional (alpha *evaluation-minimum*) (beta *evaluation-maximum*)
                &aux (original-alpha alpha))
  (when *out-of-time*
    (throw :out-of-time nil))
  (when-let ((transposition (lookup-transposition state depth)))
    ;; use stored values
    (with-slots (value type move)
        transposition
      (ccase type
        (:exact (return-from minimax (values value (list move))))
        (:lower (maxf alpha value))
        (:upper (minf beta value)))
      (when (>= alpha beta)
        (return-from minimax (values value (list move))))))
  ;; investigate the subtree
  (let ((moves (moves state)))
    (when (or (<= depth 0) (null moves))
      (return-from minimax (values (funcall evaluator state moves) '())))
    (measure-node)
    (measure-moves moves)
    (setf moves (order-moves depth ply state moves))
    (multiple-value-bind (value variation)
        (iter (for move in moves)
              (for branching-factor from 1)
              (for breadcrumb = (apply-move state move))
              (multiple-value-bind (subvalue subvariation)
                  (minimax state (1- depth) (1+ ply) evaluator
                           (evaluation-inverse beta)
                           (evaluation-inverse alpha))
                (finding (cons move subvariation)
                         maximizing (evaluation-inverse subvalue)
                         into (variation value)))
              (unapply-move state breadcrumb)
              (maxf alpha value)
              (when (>= alpha beta)
                (store-killer ply move)
                (finish))
              (finally
               (measure-branching-factor branching-factor)
               (return (values value variation))))
      ;; store transposition
      (when (>= depth *transposition-minimum-depth*)
        (with-slots ((stored-depth depth)
                     (stored-value value)
                     (stored-type type)
                     (stored-move move))
            (ensure-transposition state)
          ;; if the found value is outside or on the border of the search window,
          ;; it only proves a bound
          (cond ((<= value original-alpha)
                 (setf stored-type :upper))
                ((<= beta value)
                 (setf stored-type :lower))
                (t
                 (setf stored-type :exact)))
          (setf stored-value value
                stored-depth depth
                stored-move (first variation))))
      (values value variation))))

(defun minimax-decision (state &key (updater #'no-op) (committer #'no-op) (evaluator #'evaluate-state))
  ;; use a fresh table with every top-level search
  (let (value
        variation
        (*transposition-table* (make-transposition-table))
        (*killers* (make-killers))
        (*minimax-statistics* (make-minimax-statistics))
        (state (copy-state state)))
    (catch :out-of-time
      (iter (for max-depth from 0 below *minimax-maximum-depth*)
            (multiple-value-setq (value variation) (minimax state max-depth 0 evaluator))
            (print (list max-depth value variation))
            (unless *out-of-time* ;; this *could* happen...
              (funcall updater (first variation)))))
    (funcall committer)
    (values (first variation) variation *minimax-statistics*)))

(defun evaluation-decision (state &key (evaluator #'evaluate-state))
  (iter (for move in (moves state))
        (for breadcrumb = (apply-move state move))
        (finding move maximizing (funcall evaluator state (moves state))
                 into (best-move value))
        (unapply-move state breadcrumb)
        (finally (return best-move))))

(defstruct (mcts-node (:constructor nil))
  (move nil)
  (parent nil :type (or null mcts-node))
  (children '() :type list)
  (nwins 0 :type integer)
  (nvisits 0 :type integer)
  (untried-moves '() :type list)
  (last-player nil :type (or null player)))

(defun make-mcts-node (state &optional move parent)
  (let ((node (make-instance 'mcts-node)))
    (with-slots ((m move) (p parent) untried-moves last-player) node
      (setf m move
            p parent
            untried-moves (moves state)
            last-player (opponent (state-player state))))
    node))

(defun uct-child (node)
  (with-slots (children (parent-nvisits nvisits)) node
    (labels ((uct-score (node)
               (with-slots (nwins nvisits) node
                   (+ (/ nwins nvisits)
                      (sqrt (/ (* 2 (log parent-nvisits))
                               nvisits))))))
      (extremum children #'> :key #'uct-score))))

(defun add-child (parent move state)
  (let ((node (make-mcts-node state move parent)))
    (with-slots (untried-moves children) parent
      (setf untried-moves (delete move untried-moves :test #'move-equal))
      (push node children))
    node))

(defun mcts-update (node winner)
  (with-slots (last-player nvisits nwins) node
    (incf nvisits)
    (incf nwins (if (null winner)
                    1/2
                    (if (eq winner last-player)
                        1
                        0)))))

(defparameter *mcts-maximum-depth* 200)
(defun mcts-decision (root-state &optional (n most-positive-fixnum))
  (assert (not (state-endp root-state)))
  (let ((root-node (make-mcts-node root-state)))
    (labels ((select (node state)
               (iter (while (and (emptyp (mcts-node-untried-moves node))
                                 (not (emptyp (mcts-node-children node)))))
                     (setq node (uct-child node))
                     (apply-move state (mcts-node-move node)))
               node)
             (expand (node state)
               (with-slots (untried-moves) node
                 (unless (emptyp untried-moves)
                   (let ((move (random-elt untried-moves)))
                     (apply-move state move)
                     (setf node (add-child node move state)))))
               node)
             (rollout (state)
               (iter (for moves = (moves state))
                     (for depth from 0)
                     (until (emptyp moves))
                     ;; no winner
                     (when (> depth *mcts-maximum-depth*)
                       (return-from rollout nil))
                     (apply-move state (random-elt moves)))
               (state-winner state))
             (backprop (node winner)
               (iter (while node)
                     (mcts-update node winner)
                     (setq node (mcts-node-parent node)))))
      (iter (repeat n)
            (until *out-of-time*)
            (for node = root-node)
            (for state = (copy-state root-state))
            (setf node (select node state))
            (setf node (expand node state))
            (backprop node (rollout state))))
    (with-slots (children nvisits nwins) root-node
      (print (list :nvisits nvisits :nwins nwins))
      (mcts-node-move (extremum children #'> :key #'mcts-node-nvisits)))))
