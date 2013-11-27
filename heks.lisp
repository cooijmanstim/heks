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
               (computer-move (algorithm)
                 (setq computer-deciding t)
                 (redraw)
                 (sb-thread:make-thread
                  (lambda ()
                    (time-limited 10 (ccase algorithm
                                       (:minimax #'minimax-move)
                                       (:mcts #'mcts-move))))
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
                    (computer-move :mcts)))))
          (:mouse-button-up-event
           (:button button :x x :y y)
          (cond ((= button sdl:sdl-button-left)
                  (unless computer-deciding
                    (update-move (append submove (list (board-position (v x y)))))))
                 ((= button sdl:sdl-button-right)
                  (unless computer-deciding
                    (update-move (butlast submove 1))))))
          (:video-expose-event () (sdl:update-display)))))))


;; performs moves randomly, writing resulting states to stream every now and then.
(defun generate-states (stream n)
  (let ((storage-rate 0.05))
    (iter (with state = (make-initial-state))
          (for moves = (moves state))
          (while (> n 0))
          (when (null moves)
            (setf state (make-initial-state))
            (next-iteration))
          (apply-move state (random-elt moves))
          (when (< (random 1.0) storage-rate)
            (format-vector stream (state->vector state))
            (decf n)))))

;; use spsa to maximize win count against opponent when using the evaluation
;; function in a 1-ply minimax search
(defun learn-feature-weights (opponent initial-weights)
  (let* ((nsteps 1000)
         (nsamples 10))
    (labels ((make-player (weights)
               (lambda (state)
                 (evaluation-decision state :evaluator (make-learned-evaluator weights))))
             (performance (weights)
               (multiple-value-bind (mean stdev)
                   (measure-performance (make-player weights) opponent nsamples)
                 (values mean stdev)))
             (goodness (weights)
               (+ (* 0.1 (vector-norm weights :l 1))
                  (* 0.2 (vector-norm weights :l 2))
                  (* 0.9 (- (performance weights))))))
      (multiple-value-bind (initial-mean initial-stdev) (performance initial-weights)
        (format t "initial performance ~D (±~D)~%" initial-mean initial-stdev)
        (let ((final-weights (spsa nsteps #'goodness initial-weights :c (+ initial-stdev 1e-3))))
          (multiple-value-bind (final-mean final-stdev) (performance final-weights)
            (fresh-line)
            (format t "optimized performance from ~D (±~D) to ~D (±~D)~%"
                    initial-mean initial-stdev final-mean final-stdev)
            final-weights))))))

(defun learn-feature-weights-against-mcts ()
  (let* ((k (second (array-dimensions *featuremap-map*)))
         (initial-weights (make-sequence '(vector single-float) k :initial-element (/ 1.0 k)))
         (mcts-player (lambda (state) (mcts-decision state :max-sample-size 10000))))
    (learn-feature-weights mcts-player initial-weights)))

(defun profile-minimax ()
  (profile (lambda ()
             (time-limited 30 (lambda ()
                                (minimax-decision (make-initial-state)))))))

(defun profile-mcts ()
  (profile (lambda ()
             (time-limited 30 (lambda () (mcts-decision (make-initial-state)))))))

