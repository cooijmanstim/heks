(in-package :heks)

(declaim (optimize (debug 3) (safety 3)))

(defstruct game
  (state (make-initial-state) :type state)
  (move-history '())
  (breadcrumbs '())
  ;; list of agents, a nil agent signifies human
  (agents '())
  ;; an observer is a cons whose car is a function that is called with
  ;; state move and breadcrumb after a move is applied to state, and
  ;; whose cdr is similarly called before a move is unapplied.
  (observers '()))

(defun game-current-agent (game)
  (first (game-agents game)))

(defun game-rotate-agents (game)
  (with-slots (agents) game
    (setf agents (rotate agents -1))))

(defun game-unrotate-agents (game)
  (with-slots (agents) game
    (setf agents (rotate agents 1))))

(defun game-add-observer (game initializer updater downdater)
  (funcall initializer (game-state game))
  (push (cons updater downdater) (game-observers game)))

(defun game-add-observing-agent (game agent)
  (game-add-observer game
                     (lambda (state)
                       (initialize agent state))
                     (lambda (state move breadcrumb)
                       (update agent state move breadcrumb))
                     (lambda (state move breadcrumb)
                       (downdate agent state move breadcrumb))))

(defun game-add-agent (game agent)
  (far-push agent (game-agents game))
  (when (typep agent agent)
    (game-add-observing-agent game agent)))

(defun game-update (game move)
  (with-slots (state move-history breadcrumbs agents observers) game
    (let ((breadcrumb (apply-move state move)))
      (game-rotate-agents game)
      (iter (for observer in observers)
            (funcall (car observer) state move breadcrumb))
      (push move move-history)
      (push breadcrumb breadcrumbs))))

(defun game-downdate (game)
  (with-slots (state move-history breadcrumbs agents observers) game
    (let ((breadcrumb (pop breadcrumbs))
          (move (pop move-history)))
      (iter (for observer in observers)
            (funcall (cdr observer) state move breadcrumb))
      (game-unrotate-agents game)
      (unapply-move state breadcrumb))))

(defun game-over (game)
  (state-endp (game-state game)))

