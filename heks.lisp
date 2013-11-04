(in-package :heks)

(deftype player ()
  '(member :white :black))

(deftype tile ()
  '(member :void  ; the tile is not part of the board (an artifact of the choice
                  ; of representing the hexagonal board as a rectangular array)
           :empty ; there is no piece on the tile
           :white-man :white-king
           :black-man :black-king))

; since our choice of representation already forces us to deal with :void tiles,
; we might as well use a border of :void tiles to avoid the need for explicit
; bounds checking.  hence 11 instead of 9 here.
(defvar *board-size* 11)

(defun initial-tile (i j)
  (let* ((board-interior-size (- *board-size* 2))
         (i (- i 1)) (j (- j 1))
         ; cumulative inverse coordinates
         (k (- board-interior-size i 1))
         (l (- board-interior-size j 1)))
    (cond ((<= (+ l i) 3) :void)
          ((<= (+ k j) 3) :void)
          ; FIXME: the void cases are wrong
          ((and (<= i 3) (<= j 3)) :white-man)
          ((and (<= k 3) (<= l 3)) :black-man)
          (t :empty))))

(defun make-initial-board ()
  (let ((board (make-array (list *board-size* *board-size*)
                           :element-type 'tile
                           :initial-element :void
                           :adjustable nil)))
    ; loop over interior of board
    (loop for i from 1 below (- *board-size* 1) do
          (loop for j from 1 below (- *board-size* 1) do
                (setf (aref board i j) (initial-tile i j))))
    board))

(defun tile-character (tile)
  (ccase tile
         (:empty #\Space)
         (:white-man #\w)
         (:white-king #\W)
         (:black-man #\b)
         (:black-king #\B)))

(defun format-board (destination board)
  ; it's easier to render this beast upside down (with white up and red down)
  ; and flip it vertically later when outputting
  (let* ((horizontal-stride 2)
         (vertical-stride 1)
         (h (* 2 vertical-stride *board-size*))
         (w (* horizontal-stride *board-size*))
         (ascii-art (make-array (list h w)
                                :element-type 'character
                                :initial-element #\Space)))
    (loop for i from 0 below *board-size* do
          (loop for j from 0 below *board-size*
                for diy = vertical-stride
                for dix = (- horizontal-stride)
                for djy = vertical-stride
                for djx = horizontal-stride
                for hexy = (+ (* i diy) (* j djy))
                for hexx = (+ (* i dix) (* j djx) (floor (/ w 2)))
                for tile = (aref board i j)
                unless (or (eq tile :void) (not (array-in-bounds-p ascii-art hexy hexx)))
                do
                (setf (aref ascii-art hexy hexx)
                      (tile-character tile))))
    (loop for i from 0 below h do
          (loop for j from 0 below w do
                (format destination "~C" (aref ascii-art i j)))
          (format destination "~%"))))

;         1 a
;       2  w  b
;     3  w   w  c
;   4  w   w   w  d
; 5  w   w   w   w  e
;  .   w   w   w   .
; 6  .   w   w   .  f
;  .   .   w   .   .
; 7  .   .   .   .  g
;  .   .   .   .   .
; 8  .   .   .   .  h
;  .   .   r   .   .
; 9  .   r   r   .  i
;  .   r   r   r   .
;    r   r   r   r
;      r   r   r
;        r   r
;          r

(defstruct state
  (board (make-initial-board) :type (array tile))
  (to-move :white :type player))

