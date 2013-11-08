(in-package :heks)

(declaim (optimize (debug 3)))

(defparameter *window-width* 500)
(defparameter *window-height* 500)

(defun fraction-to-angle (fraction)
  (* 2 pi fraction))

(defparameter *scale* 24)
(defparameter *hexagon*
  (iter (for i from 0 to 6)
        (for angle = (fraction-to-angle (/ i 6)))
        (collect (mapcar (lambda (x)
                           (* x *scale*))
                         (list (cos angle) (sin angle))))))

(defparameter *hexagonal-basis*
  (mapcar (lambda (angle)
            (mapcar (lambda (x)
                      (* x 2 *scale*))
                    (list (cos angle) (sin angle))))
          (mapcar #'fraction-to-angle '(-5/12 -1/12))))

(defparameter *piece-radius* (* 0.7 *scale*))

(defparameter *background-color* (sdl:color :r #x66 :g #x66 :b #x66))
(defparameter *board-color*      (sdl:color :r #x99 :g #x99 :b #x99))
(defparameter *piece-colors*     (list (cons :white (sdl:color :r #xcc :g #xcc :b #xcc))
                                       (cons :black (sdl:color :r #x33 :g #x33 :b #x33))))

(defun onscreen-position (i j)
  (let ((is (list i j)))
    (apply #'mapcar #'+
           ;; central pixel
           (iter (for x in (list *window-width* *window-height*))
                 (collect (/ x 2)))
           (iter (for i in is)
                 (for bs in *hexagonal-basis*)
                 ;; di is i relative to the central hexagon at (5, 5)
                 (for di = (- i (floor (/ *board-size* 2))))
                 (collect (iter (for b in bs)
                                (collect (* di b))))))))

(defun draw-board (board)
  (iter (for i from 1 below (- *board-size* 1))
        (iter (for j from 1 below (- *board-size* 1))
              (draw-tile (aref board i j) (onscreen-position i j)))))

(defun draw-character (character x)
  (sdl:draw-string-solid (string character) (make-point-boa x)
                         :color *board-color* :font (sdl:initialise-font sdl:*font-8x13b*)))

(defun draw-board-axes ()
  (iter (for i from 1 below (- *board-size* 1))
        (for number in-vector "123456789")
        (for letter in-vector "abcdefghi")
        (for j in '(0 0 0 0 0 1 2 3 4))
        (draw-character number (onscreen-position i (+ j 1/4)))
        (draw-character letter (onscreen-position (+ j 1/4) i))))

(defun make-point-boa (xs)
  (sdl:point :x (first xs) :y (second xs)))

(defparameter *surface* nil)

(defun draw-tile (tile dxs)
  (unless (eq (tile-object tile) :void)
    (let ((hexagon (mapcar #'make-point-boa
                           (iter (for xs in *hexagon*)
                                 (collect (iter (for x in xs)
                                                (for dx in dxs)
                                                (collect (+ x dx)))))))
          (p (make-point-boa dxs))
          (piece-color (cdr (assoc (tile-owner tile) *piece-colors*))))
      (sdl:draw-polygon hexagon :color *board-color* :aa t)
      (sdl:flood-fill (make-point-boa dxs) :surface *surface* :color *board-color*)
      ;; draw-filled-polygon refuses to work...
      '(sdl:draw-filled-polygon hexagon :color *board-color*)
      (case (tile-object tile)
        (:man  (sdl:draw-filled-circle p (round *piece-radius*) :color piece-color))
        (:king (sdl:draw-filled-circle p (round *piece-radius*) :color piece-color)
               (sdl:draw-filled-circle p (round (* 0.8 *piece-radius*)) :color *board-color*))))))

(defun main ()
  (sdl:with-init ()
    (setq *surface* (sdl:window *window-width* *window-height*))
    (sdl:clear-display *background-color*)
    (draw-board (make-initial-board))
    (draw-board-axes)
    (sdl:update-display)
    (sdl:with-events ()
      (:quit-event () t)
      (:video-expose-event () (sdl:update-display)))))

