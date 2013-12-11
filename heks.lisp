(in-package :heks)

(declaim (optimize (debug 3)))

(defun main ()
  (graphical-game))

;; TODO: handle end-of-game
(defun graphical-game (&optional (game (let ((game (make-game)))
                                         (game-add-agent game nil))))
  (with-slots (state) game
    (let (;; UI state: submove is the partial move constructed so far. all-moves
          ;; and supermoves respectively contain all legal moves and all legal
          ;; supermoves of submove
          all-moves submove supermoves
          ;; t if one of the agents is thinking
          agent-deciding
          ;; t initially and after undo (human took over)
          (manual-operation t)
          ;; can be called upon by human
          (all-agents `((:minimax-material  ,(make-instance 'minimax-agent
                                                            :evaluator (make-instance 'simple-evaluator
                                                                                      :function #'material-evaluation)))
                        (:minimax-heuristic ,(make-instance 'minimax-agent
                                                            :evaluator (make-instance 'simple-evaluator
                                                                                      :function #'heuristic-evaluation)))
                        (:minimax-mcts      ,(make-instance 'minimax-agent
                                                            :evaluator (make-instance 'pmcts-evaluator )))
                        (:mcts              ,(make-instance 'pmcts-agent :tree (make-pmcts-tree-for-state state))))))
      (iter (for (name agent) in all-agents)
            (game-add-observing-agent game agent))
      (sdl:with-init ()
        (labels ((redraw ()
                   (sdl:clear-display *background-color*)
                   (draw-state state agent-deciding)
                   (draw-move submove)
                   (sdl:update-display))
                 (let-the-games-begin ()
                   (setf manual-operation nil)
                   (current-agent-move))
                 (current-agent-move ()
                   (when-let ((agent (game-current-agent game)))
                     (agent-move agent)))
                 (agent-move (agent &aux (agent-designator agent))
                   (setf agent-deciding t)
                   (redraw)
                   (when (keywordp agent)
                     (setf agent (second (assoc agent all-agents))))
                   (unless agent
                     (fresh-line)
                     (format t "unknown agent: ~A" agent-designator))
                   (sb-thread:make-thread
                    (lambda ()
                      ;; technically unsafe, but the consequences are mild
                      (update-move (decide agent state))
                      (setf agent-deciding nil)
                      (commit-move)
                      ;; sleeping here helps avoid threading issues
                      (sleep 0.1))
                    :name "worker thread"))
                 (fresh-move ()
                   (setf submove '()
                         all-moves (moves state)
                         supermoves (supermoves submove all-moves))
                   (when (game-over game)
                     (setf manual-operation t))
                   (redraw)
                   (fresh-line))
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
                     (game-update game submove)
                     (fresh-move)
                     (unless manual-operation
                       (current-agent-move))
                     (redraw)))
                 (undo-move ()
                   (game-downdate game)
                   (setf manual-operation t)
                   (fresh-move)))
          (setq *surface* (apply #'sdl:window (v->list *window-dimensions*)))
          (fresh-move)
          ;; interface: left-click to build a move (first click chooses the piece
          ;; to move, further clicks choose where to move it. more than one further
          ;; click corresponds to a move capturing multiple pieces, and each click
          ;; specifies a turning point.
          ;; right-click to undo the most recent left-click.
          ;; space to commit the move. backspace to undo the last move.
          ;; after undo, and at first, automated decision-making is disabled.  turn
          ;; it on with the return key.
          (sdl:with-events ()
            (:quit-event () t)
            (:key-up-event
             (:key key)
             (cond ((sdl:key= key :sdl-key-space)
                    (unless agent-deciding (commit-move)))
                   ((sdl:key= key :sdl-key-backspace)
                    (unless agent-deciding (undo-move)))
                   ((sdl:key= key :sdl-key-return)
                    (unless agent-deciding (let-the-games-begin)))
                   ((sdl:key= key :sdl-key-f5)
                    (unless agent-deciding (agent-move :minimax-material)))
                   ((sdl:key= key :sdl-key-f6)
                    (unless agent-deciding (agent-move :minimax-heuristic)))
                   ((sdl:key= key :sdl-key-f7)
                    (unless agent-deciding (agent-move :minimax-mcts)))
                   ((sdl:key= key :sdl-key-f8)
                    (unless agent-deciding (agent-move :mcts)))
                   ((sdl:key= key :sdl-key-f12)
                    ;; trigger debugger
                    (break))
                   ((sdl:key= key :sdl-key-escape)
                    (setf manual-operation t))))
            (:mouse-button-up-event
             (:button button :x x :y y)
             (cond ((= button sdl:sdl-button-left)
                    (unless agent-deciding (update-move (append submove (list (board-position (v x y)))))))
                   ((= button sdl:sdl-button-right)
                    (unless agent-deciding (update-move (butlast submove 1))))))
            (:video-expose-event () (sdl:update-display))))))))


(defun profile-agent (agent)
  (let ((state (make-initial-state)))
    (initialize agent state)
    (time-limited 30 (lambda ()
                       (profile (lambda ()
                                  (loop (decide agent state))))))))
