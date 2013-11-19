(in-package :heks)

(declaim (optimize (debug 3)))

(defparameter *minimax-decision-time* 10)

;; TODO: handle end-of-game
(defun main ()
  (let ((state (make-initial-state))
        all-moves submove supermoves breadcrumbs
        minimax-deciding)
    (sdl:with-init ()
      (labels ((redraw ()
                 (sdl:clear-display *background-color*)
                 (draw-state state minimax-deciding)
                 (draw-move submove)
                 (sdl:update-display))
               (fresh-move ()
                 (setf submove '())
                 (setq all-moves (moves state)
                       supermoves (supermoves submove all-moves))
                 (redraw))
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
                 (setq minimax-deciding t)
                 (minimax-decision (copy-state state) *minimax-decision-time*
                                   ;; FIXME: the below are called from another thread. communicate
                                   ;; through the threadsafe sdl:push-user-event instead!
                                   (lambda (move)
                                     (update-move move))
                                   (lambda ()
                                     (setq minimax-deciding nil)
                                     (commit-move)))))
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
           (:state s :scancode scancode :key key :mod mod :unicode unicode)
           (declare (ignore s scancode unicode mod))
           (cond ((sdl:key= key :sdl-key-space)
                  (unless minimax-deciding
                    (commit-move)))
                 ((sdl:key= key :sdl-key-z)
                  (unless minimax-deciding
                    (when (intersection (sdl:get-mods-state) (list :sdl-key-mod-lctrl :sdl-key-mod-rctrl))
                      (undo-move))))
                 ((sdl:key= key :sdl-key-c)
                  (unless minimax-deciding
                    (minimax-move)))))
          (:mouse-button-up-event
           (:button button :state s :x x :y y)
           (declare (ignore s))
           (cond ((= button sdl:sdl-button-left)
                  (unless minimax-deciding
                    (update-move (append submove (list (board-position (v x y)))))))
                 ((= button sdl:sdl-button-right)
                  (unless minimax-deciding
                    (update-move (butlast submove 1))))))
          (:video-expose-event () (sdl:update-display)))))))

