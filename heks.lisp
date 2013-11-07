(in-package :heks)

(declaim (optimize (debug 3)))

(deftype player ()
  '(member :white :black))

; XXX: it is assumed that owner is non-nil iff object is :man or :king
(defstruct (tile (:constructor make-tile (object &optional owner)))
  (object :void :type (member :void     ; the tile is not part of the board (necessary
                                        ; because we're cramming a hexagonal board into
                                        ; a rectangular array
                              :empty    ; there is nothing on the tile
                              :man :king))
  (owner nil :type (or nil player)))

(defstruct state
  (board (make-initial-board) :type (array tile))
  (player :white :type player))

; since our choice of representation already forces us to deal with :void tiles,
; we might as well use a border of :void tiles to avoid the need for explicit
; bounds checking.  hence 11 instead of 9 here.
(defparameter *board-size* 11)

(defun initial-tile (i j)
  (let* ((board-interior-size (- *board-size* 2))
         (i (- i 1)) (j (- j 1))
         ; cumulative inverse coordinates
         (k (- board-interior-size i 1))
         (l (- board-interior-size j 1)))
    (cond ((<= (+ l i) 3) (make-tile :void))
          ((<= (+ k j) 3) (make-tile :void))
          ((and (<= i 3) (<= j 3)) (make-tile :man :white))
          ((and (<= k 3) (<= l 3)) (make-tile :man :black))
          (t (make-tile :empty)))))

(defun make-initial-board ()
  (let ((board (make-array (list *board-size* *board-size*)
                           :element-type 'tile
                           :initial-element (make-tile :void)
                           :adjustable nil)))
    ; loop over interior of board
    (iter (for i from 1 below (- *board-size* 1))
          (iter (for j from 1 below (- *board-size* 1))
                (setf (aref board i j) (initial-tile i j))))
    board))

(defun make-empty-board ()
  (let ((board (make-initial-board)))
    (iter (for i from 1 below (- *board-size* 1))
          (iter (for j from 1 below (- *board-size* 1))
                (unless (eq (tile-object (aref board i j)) :void)
                  (setf (aref board i j) (make-tile :empty)))))
    board))

(defun tile-character (tile)
  (let ((object (tile-object tile))
        (owner  (tile-owner  tile)))
    (if (eq object :empty)
        #\Space
        (if (eq owner :white)
            (if (eq object :man) #\w #\W)
            (if (eq object :man) #\b #\B)))))

(defun format-board (destination board)
  ; it's easier to render this beast upside down (with white up and black down)
  ; and flip it vertically later when outputting
  (let* ((horizontal-stride 2)
         (vertical-stride 1)
         (h (* 2 vertical-stride *board-size*))
         (w (* horizontal-stride *board-size*))
         (ascii-art (make-array (list h w)
                                :element-type 'character
                                :initial-element #\Space))
         (diy vertical-stride)
         (dix (- horizontal-stride))
         (djy vertical-stride)
         (djx horizontal-stride))
    (iter (for i from 0 below *board-size*)
          (iter (for j from 0 below *board-size*)
                (for hexy = (+ (* i diy) (* j djy)))
                (for hexx = (+ (* i dix) (* j djx) (floor (/ w 2))))
                (for tile = (aref board i j))
                (unless (or (eq (tile-object tile) :void)
                            (not (array-in-bounds-p ascii-art hexy hexx)))
                  (setf (aref ascii-art hexy hexx)
                        (tile-character tile)))))
    (iter (for i from 0 below h)
          (iter (for j from 0 below w)
                (format destination "~C" (aref ascii-art i j)))
          (format destination "~%"))))

(defvar *all-directions* '((-1 .  0) (0 . -1)
                           ( 0 .  1) (1 .  0)
                           (-1 . -1) (1 .  1)))
;; using {0,1} or {-1,1} for players instead of :white :black would make this unnecessary
(defun player-forward-directions (player)
  (ccase player
    (:white '((0 .  1) ( 1 . 0) ( 1 .  1)))
    (:black '((0 . -1) (-1 . 0) (-1 . -1)))))
(defun opponent (player)
  (ccase player
    (:white :black)
    (:black :white)))

(defun piece-properties (object player)
  (ccase object
    (:man (list (player-forward-directions player) 1))
    (:king (list *all-directions* *board-size*))))

(defun piece-moves (board player i j)
  (destructuring-bind (directions maximum-distance)
      (piece-properties (tile-object (aref board i j)) player)
    (iter (for didj in directions)
          (nconcing (iter (for d from 1 to maximum-distance)
                          (for i2 = (+ i (* d (car didj))))
                          (for j2 = (+ j (* d (cdr didj))))
                          (while (eq (tile-object (aref board i2 j2)) :empty))
                          (collect (list (cons i j) (cons i2 j2))))))))

(defun piece-captures (board player i j)
  (destructuring-bind (directions maximum-distance)
      (piece-properties (tile-object (aref board i j)) player)
    (declare (ignore directions)) ; capture goes in all directions
    (labels ((poop (capture-points)
               (iter (for didj in *all-directions*)
                     (with captures = '())
                     (iter inner-loop
                           (for d from 1 to maximum-distance)
                           (for i2 = (+ i (* d (car didj))))
                           (for j2 = (+ j (* d (cdr didj))))
                           (for tile = (aref board i2 j2))
                           (for object = (tile-object tile))
                           (for owner = (tile-owner tile))
                           (with capture-point = nil)
                           (cond ((and (null capture-point)
                                       (eq object :empty))
                                  ;; haven't yet passed over an enemy piece. no capture, but keep going.
                                  )
                                 ((and (null capture-point)
                                       (member object '(:man :king))
                                       (eq owner (opponent player))
                                       (not (member (cons i2 j2) capture-points :test #'tree-equal)))
                                  ;; passing over a fresh enemy piece.
                                  (setf capture-point (cons i2 j2)))
                                 ((and capture-point
                                       (eq object :empty))
                                  ;; have passed over an enemy piece. possible stopping point or turning point.
                                  (push (list (cons i j) (cons i2 j2)) captures)
                                  (iter (for continuation in (poop (cons capture-point capture-points)))
                                        (push (cons (cons i j) continuation) captures)))
                                 (t
                                  ;; this is going nowhere
                                  (return-from inner-loop))))
                     (finally (return captures)))))
      (poop '()))))

;;; NOTE: a move is a list (x y z ...) of points visited,
;;; and everything between the points is captured
;;; NOTE: capturing is mandatory, so if captures have been found, we skip
;;; finding moves
(defun moves (state)
  (let ((board    (state-board    state))
        (player   (state-player   state)))
    (iter (for i from 1 below (- *board-size* 1))
          (for subresult = (iter (for j from 1 below (- *board-size* 1))
                                 (for tile = (aref board i j))
                                 (for owner = (tile-owner tile))
                                 (when (eq owner player)
                                   (nconcing (piece-captures board player i j) into captures)
                                   (unless captures
                                     (nconcing (piece-moves board player i j) into moves)))
                                 (finally (return (cons captures moves)))))
          (nconcing (car subresult) into captures)
          (unless captures
            (nconcing (cdr subresult) into moves))
          (finally
           (if (null captures)
               moves
               ;; longest capture is mandatory
               (cl-utilities:extrema captures #'> :key #'length))))))

