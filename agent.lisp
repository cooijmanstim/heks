(in-package :heks)

;; TODO: time

(defclass agent () ())

(defgeneric initialize (agent state)
  (:method ((agent agent) state)))
(defgeneric update   (agent state move breadcrumb)
  (:method ((agent agent) state move breadcrumb)))
(defgeneric downdate (agent state move breadcrumb)
  (:method ((agent agent) state move breadcrumb)))
(defgeneric decide (agent state))

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

(defmethod decide ((agent minimax-agent) state)
  ;; TODO: use an instance variable instead of global *out-of-time*
  (time-limited 10 (lambda ()
                     (minimax-decision state :evaluator (minimax-agent-evaluator agent)))))

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

(defmethod decide ((agent pmcts-agent) state)
  (time-limited 10 (lambda ()
                     (mcts-decision state :root-node (pmcts-tree-current-node (pmcts-agent-tree agent))))))

