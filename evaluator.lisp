(in-package :heks)

(declaim (optimize (debug 3) (safety 3)))

(defclass evaluator () ())

;; called with the initial state
(defgeneric evaluation* (evaluator state)
  (:method ((evaluator evaluator) state)))
;; called after a move is applied to state
(defgeneric evaluation+ (evaluator state move breadcrumb)
  (:method ((evaluator evaluator) state move breadcrumb)))
;; called before a move is unapplied to state
(defgeneric evaluation- (evaluator state move breadcrumb)
  (:method ((evaluator evaluator) state move breadcrumb)))
;; called to evaluate a state
(defgeneric evaluation? (evaluator state moves))

(defclass simple-evaluator (evaluator)
  ((fn :type function
       :initarg :function)))

(defmethod evaluation? ((evaluator simple-evaluator) state moves)
  (if (null moves)
      *evaluation-minimum*
      (funcall (slot-value evaluator 'fn) state moves)))

(defun make-simple-evaluator (fn)
  (make-instance 'simple-evaluator :function fn))

(defclass material-evaluator (evaluator)
  ((material :initform (make-running-material)
             :type running-material)))

(defmethod evaluation* ((evaluator material-evaluator) state)
  (initialize-running-material (slot-value evaluator 'material) state))
(defmethod evaluation+ ((evaluator material-evaluator) state move breadcrumb)
  (update-running-material (slot-value evaluator 'material) state move breadcrumb))
(defmethod evaluation- ((evaluator material-evaluator) state move breadcrumb)
  (downdate-running-material (slot-value evaluator 'material) state move breadcrumb))
(defmethod evaluation? ((evaluator material-evaluator) state moves)
  (if (null moves)
      *evaluation-minimum*
      (get-material-advantage (slot-value evaluator 'material) (state-player state))))

;; an evaluator that can be reused and will get better as it is reused.
;; uses evaluation and pondering time to build an mcts tree.
(defclass pmcts-evaluator (evaluator)
  ((tree :type pmcts-tree)
   ;; running average of number of samples informing evaluations
   (support-mean :initform 0.0 :type single-float)
   (support-sample-size :initform 0 :type integer)
   (prior-evaluation :initform (make-instance 'material-evaluator) :type evaluator)))

(defmethod evaluation* ((evaluator pmcts-evaluator) state)
  (setf (slot-value evaluator 'tree) (make-pmcts-tree-for-state state))
  (evaluation* (slot-value evaluator 'prior-evaluation) state))

(defmethod evaluation+ ((evaluator pmcts-evaluator) state move breadcrumb)
  (update-pmcts-tree (slot-value evaluator 'tree) state move breadcrumb)
  (evaluation+ (slot-value evaluator 'prior-evaluation) state move breadcrumb))
(defmethod evaluation- ((evaluator pmcts-evaluator) state move breadcrumb)
  (downdate-pmcts-tree (slot-value evaluator 'tree) state move breadcrumb)
  (evaluation- (slot-value evaluator 'prior-evaluation) state move breadcrumb))

(defmethod evaluation? ((evaluator pmcts-evaluator) state moves)
  (declare (optimize (speed 3) (safety 1)))
  (if (null moves)
      *evaluation-minimum*
      (with-slots (tree support-mean support-sample-size) evaluator
        (declare (single-float support-mean)
                 (integer support-sample-size))
        (with-slots (current-node) tree
          (assert (= (state-hash state) (mcts-node-state-hash current-node)))
          (dotimes (i (- 10 (mcts-node-nvisits current-node)))
            (mcts-sample current-node state))
          (update-running-average support-mean support-sample-size (mcts-node-nvisits current-node))
          (let ((prior (sigmoid (* 1/4 (evaluation? (slot-value evaluator 'prior-evaluation) state moves)))))
            (round (* 100 (- (mcts-node-win-probability (mcts-node-best-child current-node) prior)
                             0.5))))))))

(defun pmcts-evaluator-sample (evaluator state)
  (pmcts-sample (slot-value evaluator 'tree) state))

(defun pmcts-evaluator-uproot (evaluator level)
  (pmcts-tree-uproot (slot-value evaluator 'tree) level))
