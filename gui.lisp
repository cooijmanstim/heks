(in-package :heks)

(declaim (optimize (debug 3)))

(defparameter *window-dimensions* (v 500 500))
(defparameter *window-center* (scale-vector 1/2 *window-dimensions*))

(defparameter *surface* nil)

(defparameter *font* (sdl:initialise-font sdl:*font-10x20*))
(defparameter *font-dimensions* (v (sdl:char-width  *font*)
                                   (sdl:char-height *font*)))

(defparameter *status-position* (sdl:point :x 10 :y (- (s2 *window-dimensions*) 10 (s2 *font-dimensions*))))

(defparameter *background-color* (sdl:color :r #x66 :g #x66 :b #x66))
(defparameter *board-color*      (sdl:color :r #x88 :g #x88 :b #x88))
(defparameter *piece-colors*     (vector (sdl:color :r #xcc :g #xcc :b #xcc)
                                         (sdl:color :r #x33 :g #x33 :b #x33)))
(defparameter *status-color*     (sdl:color :r #xaa :g #xaa :b #xaa))

(defparameter *move-color* (sdl:color :r #x33 :g #x55 :b #x77))

(defun fraction->angle (fraction)
  (* 2 pi fraction))

(defparameter *scale* 24)
(defparameter *hexagon*
  (iter (for i from 0 to 6)
        (collect (scale-vector *scale* (angle->vector (fraction->angle (/ i 6)))))))

(defparameter *hexagonal-basis*
  (matrix (transpose-lists
           (mapcar (lambda (fraction)
                     (scale-vector (* 2 *scale*)
                                   (angle->vector (fraction->angle fraction))
                                   'list))
                   ;; negative angles because screen y is upside-down
                   '(-5/12 -1/12)))))

(defparameter *piece-radius* (* 0.7 *scale*))
(defparameter *move-radius* (round (* 0.2 *scale*)))

(defun window-position (ij)
  (let ((ij (vector-minus ij *board-center*)))
    (vector-plus (matrix-dot-vector *hexagonal-basis* ij)
                 *window-center*)))

(defun board-position (xy)
  (let ((xy (vector-minus xy *window-center*)))
    (map 'v #'round
         (vector-plus (matrix-dot-vector (matrix-inverse *hexagonal-basis*) xy)
                      *board-center*))))

(defun draw-board (board)
  (iter (for tile at ij of board)
        (draw-tile tile (window-position ij))))

(defun draw-character (character xs)
  (let ((xs (vector-minus xs (scale-vector 1/2 *font-dimensions*))))
    (sdl:draw-string-solid (string character) (vector->sdlpoint xs)
                           :color *board-color* :font *font*)))

(defun draw-board-axes ()
  (iter (for i from 1 below (- *board-size* 1))
        (for number in-vector "123456789")
        (for letter in-vector "abcdefghi")
        (for j in '(0 0 0 0 0 1 2 3 4))
        (draw-character number (window-position (vector (+ j 1/4) i)))
        (draw-character letter (window-position (vector i (+ j 1/4))))))

(defun draw-tile (tile dx)
  (with-slots (object owner) tile
    (unless (eq object :void)
      (let ((hexagon (iter (for x in *hexagon*)
                           (collect (vector->sdlpoint (vector-plus x dx)))))
            (center (vector->sdlpoint dx))
            (piece-color (when owner
                           (svref *piece-colors* owner))))
        (sdl:draw-polygon hexagon :color *board-color* :aa t)
        (sdl:flood-fill center :surface *surface* :color *board-color*)
        ;; draw-filled-polygon refuses to work...
        '(sdl:draw-filled-polygon hexagon :color *board-color*)
        (case object
          (:man  (sdl:draw-filled-circle center (round *piece-radius*) :color piece-color))
          (:king (sdl:draw-filled-circle center (round *piece-radius*) :color piece-color)
                 (sdl:draw-filled-circle center (round (* 0.8 *piece-radius*)) :color *board-color*)))))))

(defun draw-status (status)
  (sdl:draw-string-solid status *status-position*
                         :color *status-color* :font *font*))

(defun draw-state (state minimax-deciding)
  (draw-board (state-board state))
  (draw-board-axes)
  (draw-status (format nil "~A to move~A"
                       (player-name (state-player state))
                       (if minimax-deciding
                           "; thinking..."
                           ""))))

(defun draw-move (move)
  (iter (for b in (mapcar (compose #'vector->sdlpoint #'window-position) move))
        (for a previous b)
        (sdl:draw-filled-circle b *move-radius* :surface *surface* :color *move-color*)
        (unless (null a)
          (sdl:draw-line a b :surface *surface* :color *move-color* :aa t))))
