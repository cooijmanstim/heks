(in-package :heks)

(declaim (optimize (debug 3)))

(defparameter *window-dimensions* (v 500 500))
(defparameter *window-center* (s*v 1/2 *window-dimensions*))

(defparameter *surface* nil)

(defparameter *font* (sdl:initialise-font sdl:*font-10x20*))
(defparameter *font-dimensions* (v (sdl:char-width  *font*)
                                        (sdl:char-height *font*)))

(defparameter *status-position* (sdl:point :x 10 :y (- (s2 *window-dimensions*) 10 (s2 *font-dimensions*))))

(defparameter *background-color* (sdl:color :r #x66 :g #x66 :b #x66))
(defparameter *board-color*      (sdl:color :r #x88 :g #x88 :b #x88))
(defparameter *piece-colors*     (list (cons :white (sdl:color :r #xcc :g #xcc :b #xcc))
                                       (cons :black (sdl:color :r #x33 :g #x33 :b #x33))))
(defparameter *status-color*     (sdl:color :r #xaa :g #xaa :b #xaa))

(defun fraction->angle (fraction)
  (* 2 pi fraction))

(defparameter *scale* 24)
(defparameter *hexagon*
  (iter (for i from 0 to 6)
        (collect (s*v *scale* (angle->v (fraction->angle (/ i 6)))))))

(defparameter *hexagonal-basis*
  (mapcar (lambda (fraction)
            (s*v (* 2 *scale*)
                 (angle->v (fraction->angle fraction))))
          ;; negative angles because screen y is upside-down
          '(-5/12 -1/12)))

(defparameter *hexagonal-basis-inverse*
  (let* ((a (s1 (first  *hexagonal-basis*)))
         (b (s1 (second *hexagonal-basis*)))
         (c (s2 (first  *hexagonal-basis*)))
         (d (s2 (second *hexagonal-basis*)))
         (det (- (* a d) (* b c))))
    (iter (for v in (list (v d (- c))
                          (v (- b) a)))
          (collect (s*v (/ 1 det) v)))))

(defparameter *piece-radius* (* 0.7 *scale*))

(defun window-position (ij)
  (let ((ij (v-v ij *board-center*)))
    (v+v (v+v (s*v (s1 ij) (first  *hexagonal-basis*))
              (s*v (s2 ij) (second *hexagonal-basis*)))
         *window-center*)))

(defun board-position (xy)
  (let ((xy (v-v xy *window-center*)))
    (vmap #'round
          (v+v (v+v (s*v (s1 xy) (first  *hexagonal-basis-inverse*))
                    (s*v (s2 xy) (second *hexagonal-basis-inverse*)))
               *board-center*))))

(defun draw-board (board)
  (iter (for i from 1 below (- *board-size* 1))
        (iter (for j from 1 below (- *board-size* 1))
              (draw-tile (aref board i j) (window-position (v i j))))))

(defun draw-character (character xs)
  (let ((xs (v-v xs (s*v 1/2 *font-dimensions*))))
    (sdl:draw-string-solid (string character) (v->sdlpoint xs)
                           :color *board-color* :font *font*)))

(defun draw-board-axes ()
  (iter (for i from 1 below (- *board-size* 1))
        (for number in-vector "123456789")
        (for letter in-vector "abcdefghi")
        (for j in '(0 0 0 0 0 1 2 3 4))
        (draw-character number (window-position (v (+ j 1/4) i)))
        (draw-character letter (window-position (v i (+ j 1/4))))))

(defun draw-tile (tile dx)
  (unless (eq (tile-object tile) :void)
    (let ((hexagon (mapcar #'v->sdlpoint
                           (iter (for x in *hexagon*)
                                 (collect (v+v x dx)))))
          (center (v->sdlpoint dx))
          (piece-color (cdr (assoc (tile-owner tile) *piece-colors*))))
      (sdl:draw-polygon hexagon :color *board-color* :aa t)
      (sdl:flood-fill (v->sdlpoint dx) :surface *surface* :color *board-color*)
      ;; draw-filled-polygon refuses to work...
      '(sdl:draw-filled-polygon hexagon :color *board-color*)
      (case (tile-object tile)
        (:man  (sdl:draw-filled-circle center (round *piece-radius*) :color piece-color))
        (:king (sdl:draw-filled-circle center (round *piece-radius*) :color piece-color)
               (sdl:draw-filled-circle center (round (* 0.8 *piece-radius*)) :color *board-color*))))))

(defun draw-status (status)
  (sdl:draw-string-solid status *status-position*
                         :color *status-color* :font *font*))

(defun draw-state (state)
  (draw-board (state-board state))
  (draw-board-axes)
  (draw-status (format nil "~A to move" (state-player state))))

(defun submovep (sub super)
  (iter (for a on sub)
        (for b on super)
        (for sub-has-more = (rest a))
        ;; iter immediately returns nil if this form fails
        (always (v= (first a) (first b)))
        ;; if it doesn't, iter stops when it reaches the end of the shortest
        ;; list; see which one it is. for submovep it is required that super
        ;; is the longer list.
        (finally (return (not sub-has-more)))))

;; those candidates that sub is a submove to
;; use case: user clicks around and builds up a partial move, this will
;; return the valid moves that the user can click together from there
(defun supermoves (sub candidates)
  (iter (for candidate in candidates)
        (when (submovep sub candidate)
          (collect candidate))))

(defun main ()
  (let ((state (make-initial-state))
        all-moves submove supermoves breadcrumbs)
    (sdl:with-init ()
      (labels ((recompute-moves ()
                 (setq all-moves (moves state)
                       supermoves (supermoves submove all-moves))
                 (print supermoves))
               (redraw ()
                 (sdl:clear-display *background-color*)
                 (draw-state state)
                 (sdl:update-display)))
        (setq *surface* (apply #'sdl:window (v->list *window-dimensions*)))
        (recompute-moves)
        (redraw)
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
           (declare (ignore s scancode unicode))
           (cond ((sdl:key= key :sdl-key-space)
                  (when (and submove supermoves)
                    (push (apply-move state submove) breadcrumbs)
                    (setf submove '())
                    (recompute-moves)
                    (redraw)))
                 ((sdl:key= key :sdl-key-z)
                  (when (intersection mod (list :sdl-key-mod-lctrl :sdl-key-mod-rctrl))
                    (unapply-move state (pop breadcrumbs))
                    (setf submove '())
                    (recompute-moves)
                    (redraw)))))
          (:mouse-button-up-event
           (:button button :state s :x x :y y)
           (declare (ignore s))
           ;; can't use (case button ...) because sdl:sdl-button-left is of type 'bit -_-
           (cond ((= button sdl:sdl-button-left)
                  (let ((ij (board-position (v x y))))
                    ;; tentatively push
                    (far-push ij submove)
                    (recompute-moves)
                    ;; if no supermoves, then the move is invalid
                    (cond ((null supermoves)
                           (far-pop submove)
                           (recompute-moves))
                          (t (redraw)))))
                 ((= button sdl:sdl-button-right)
                  (far-pop submove)
                  (recompute-moves)
                  (redraw))))
          (:video-expose-event () (sdl:update-display)))))))

