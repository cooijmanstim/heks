(in-package :heks)

(declaim (optimize (debug 3)))

;; TODO: handle end-of-game
(defun main (&optional (state (make-initial-state)) (breadcrumbs '()))
  (let (all-moves submove supermoves computer-deciding)
    (sdl:with-init ()
      (labels ((redraw ()
                 (sdl:clear-display *background-color*)
                 (draw-state state computer-deciding)
                 (draw-move submove)
                 (sdl:update-display))
               (fresh-move ()
                 (setf submove '())
                 (setq all-moves (moves state)
                       supermoves (supermoves submove all-moves))
                 (redraw)
                 (fresh-line)
                 (format t "state evaluation: ~A" (evaluate-state state all-moves)))
               (update-move (new-submove)
                 (print (list :update-move new-submove))
                 (let ((new-supermoves (supermoves new-submove all-moves)))
                   (cond ((null new-supermoves)
                          ;; TODO: handle this
                          (fresh-line)
                          (format t "invalid move: ~A" new-submove))
                         (t
                          (setf submove new-submove
                                supermoves new-supermoves)
                          (redraw)))))
               (commit-move ()
                 (print (list :commit-move))
                 (when (and submove
                            (set-equal (list submove) supermoves :test #'move-equal))
                   (push (apply-move state submove) breadcrumbs)
                   (fresh-move)))
               (undo-move ()
                 (unapply-move state (pop breadcrumbs))
                 (fresh-move))
               (minimax-move ()
                 (minimax-decision state
                                   ;; XXX: this is not threadsafe, but we'll live with it.  the other
                                   ;; option would be to communicate the information to the event thread
                                   ;; with sdl:push-user-event, but we can only pass a couple of numbers
                                   ;; through there.  it's not worth the trouble.
                                   :updater (lambda (move)
                                              (update-move move))
                                   :committer (lambda ()
                                                (setq computer-deciding nil)
                                                (commit-move))))
               (mcts-move ()
                 (update-move (mcts-decision state))
                 (setq computer-deciding nil)
                 (commit-move))
               (evaluation-move ()
                 (update-move (evaluation-decision state))
                 (setq computer-deciding nil)
                 (commit-move))
               (computer-move (algorithm)
                 (setq computer-deciding t)
                 (redraw)
                 (sb-thread:make-thread
                  (lambda ()
                    (time-limited 10 (ccase algorithm
                                       (:minimax #'minimax-move)
                                       (:mcts #'mcts-move)
                                       (:evaluation #'evaluation-move))))
                  :name "worker thread")))
        (setq *surface* (apply #'sdl:window (v->list *window-dimensions*)))
        (fresh-move)
        ;; interface: left-click to build a move (first click chooses the piece
        ;; to move, further clicks choose where to move it. more than one further
        ;; click corresponds to a move capturing multiple pieces, and each click
        ;; specifies a turning point.
        ;; right-click to undo the most recent left-click.
        ;; space to commit the move. ctrl-z to undo the last move.
        (sdl:with-events ()
          (:quit-event () t)
          (:key-up-event
           (:key key)
           (cond ((sdl:key= key :sdl-key-space)
                  (unless computer-deciding
                    (commit-move)))
                 ((sdl:key= key :sdl-key-z)
                  (unless computer-deciding
                    (when (intersection (sdl:get-mods-state) (list :sdl-key-mod-lctrl :sdl-key-mod-rctrl))
                      (undo-move))))
                 ((sdl:key= key :sdl-key-f1)
                  (unless computer-deciding
                    (computer-move :minimax)))
                 ((sdl:key= key :sdl-key-f2)
                  (unless computer-deciding
                    (computer-move :mcts)))
                 ((sdl:key= key :sdl-key-f3)
                  (unless computer-deciding
                    (computer-move :evaluation)))
                 ((sdl:key= key :sdl-key-f12)
                  (break))))
          (:mouse-button-up-event
           (:button button :x x :y y)
          (cond ((= button sdl:sdl-button-left)
                 (unless computer-deciding
                   (update-move (append submove (list (board-position (v x y)))))))
                ((= button sdl:sdl-button-right)
                 (unless computer-deciding
                   (update-move (butlast submove 1))))))
          (:video-expose-event () (sdl:update-display)))))))


(defun profile-minimax ()
  (profile (lambda ()
             (time-limited 30 (lambda ()
                                (minimax-decision (make-initial-state)))))))

(defun profile-mcts ()
  (profile (lambda ()
             (time-limited 30 (lambda () (mcts-decision (make-initial-state)))))))

