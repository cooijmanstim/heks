(in-package :heks)

;; TODO: time

(defclass agent () ())

(defgeneric decide   (agent state))
(defgeneric update   (agent state move breadcrumb)
  (:method ((agent agent) state move breadcrumb)))
(defgeneric downdate (agent state move breadcrumb)
  (:method ((agent agent) state move breadcrumb)))

(defclass minimax-agent (agent)
  ((evaluator :type evaluator
              :initform (make-material-evaluator)
              :initarg :evaluator
              :accessor minimax-agent-evaluator)))

(defmethod decide ((agent minimax-agent) state)
  ;; TODO: use an instance variable instead of global *out-of-time*
  (time-limited 10 (lambda ()
                     (minimax-decision state :evaluator (minimax-agent-evaluator agent)))))

(defmethod update   ((agent minimax-agent) state move breadcrumb)
  (evaluation+ (minimax-agent-evaluator agent) state move breadcrumb))

(defmethod downdate ((agent minimax-agent) state move breadcrumb)
  (evaluation- (minimax-agent-evaluator agent) state move breadcrumb))

(defclass pmcts-agent (agent)
  ((tree :type pmcts-tree
         :initarg :tree
         :accessor pmcts-agent-tree)))

(defmethod decide ((agent pmcts-agent) state)
  (time-limited 10 (lambda ()
                     (mcts-decision state))))

(defmethod update   ((agent pmcts-agent) state move breadcrumb)
  (update-pmcts-tree (pmcts-agent-tree agent) state move breadcrumb))

(defmethod downdate ((agent pmcts-agent) state move breadcrumb)
  (downdate-pmcts-tree (pmcts-agent-tree agent) state move breadcrumb))
