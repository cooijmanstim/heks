(in-package :heks)

(declaim (optimize (debug 3)))


;; TODO: use a more practical representation such as {-1, 1}
(deftype player ()
  '(member :white :black))

(defun player-forward-directions (player)
  (ccase player
    (:white (mapcar #'list->v '((0  1) ( 1 0) ( 1  1))))
    (:black (mapcar #'list->v '((0 -1) (-1 0) (-1 -1))))))

(defun opponent (player)
  (ccase player
    (:white :black)
    (:black :white)))


(defun can-fly (object)
  (ccase object
    (:man nil)
    (:king t)))

(defun crown-p (tile ij)
  (with-slots (object owner) tile
    (and (eq object :man)
         (ccase owner
           (:black (or (= (s1 ij) 1) (= (s2 ij) 1)))
           (:white (or (= (s1 ij) (- *board-size* 2))
                       (= (s2 ij) (- *board-size* 2))))))))


; XXX: it is assumed that owner is non-nil iff object is :man or :king
(defstruct (tile (:constructor make-tile (object &optional owner)) (:copier nil))
  (object :void :type (member :void     ; the tile is not part of the board (necessary
                                        ; because we're cramming a hexagonal board into
                                        ; a rectangular array
                              :empty    ; there is nothing on the tile
                              :man :king))
  (owner nil :type (or nil player)))

(defun initial-tile (ij)
  (let* ((board-interior-dimensions (s+v -2 *board-dimensions*))
         (ij (s+v -1 ij))
         ;; cumulative inverse coordinates
         (kl (s+v -1 (v-v board-interior-dimensions ij))))
    (cond ((or (<= (+ (s2 kl) (s1 ij)) 3)
               (<= (+ (s1 kl) (s2 ij)) 3)) (make-tile :void))
          ((absv<=s ij 3) (make-tile :man :white))
          ((absv<=s kl 3) (make-tile :man :black))
          (t (make-tile :empty)))))

(defun copy-tile (tile)
  (make-tile (tile-object tile) (tile-owner tile)))

(defun tile-equal (a b)
  (and (eq (tile-object a) (tile-object b))
       (eq (tile-owner  a) (tile-owner  b))))


; since our choice of representation already forces us to deal with :void tiles,
; we might as well use a border of :void tiles to avoid the need for explicit
; bounds checking.  hence 11 instead of 9 here.
(defparameter *board-size* 11)
(defparameter *board-dimensions* (v *board-size* *board-size*))
(defparameter *board-center* (s+v -1/2 (s*v 1/2 *board-dimensions*)))

(defun board-tile (board ij)
  (aref board (s1 ij) (s2 ij)))
(defun set-board-tile (board ij value)
  (setf (aref board (s1 ij) (s2 ij)) value))
(defsetf board-tile set-board-tile)

(defun make-initial-board ()
  (let ((board (make-array (v->list *board-dimensions*)
                           :element-type 'tile
                           :initial-element (make-tile :void)
                           :adjustable nil)))
    (iter (for i from 1 below (- *board-size* 1))
          (iter (for j from 1 below (- *board-size* 1))
                (setf (aref board i j) (initial-tile (v i j)))))
    board))

(defun make-empty-board ()
  (let ((board (make-initial-board)))
    (iter (for i from 1 below (- *board-size* 1))
          (iter (for j from 1 below (- *board-size* 1))
                (unless (eq (tile-object (aref board i j)) :void)
                  (setf (aref board i j) (make-tile :empty)))))
    board))

(defun copy-board (board)
  (let ((board2 (make-array (array-dimensions board)
                            :element-type 'tile
                            :adjustable nil)))
    (iter (for i from 0 below *board-size*)
          (iter (for j from 0 below *board-size*)
                (setf (aref board2 i j) (copy-tile (aref board i j)))))
    board2))

(defun board-equal (a b)
  (iter (for i from 0 below *board-size*)
        (iter (for j from 0 below *board-size*)
              (always (tile-equal (aref a i j) (aref b i j))))))

(defstruct (state (:constructor nil) (:copier nil))
  (board #() :type (array tile))
  (player :white :type player)
  (hash 0 :type integer))

(defun make-state (&optional
                   (board (make-initial-board))
                   (player :white)
                   (hash (logxor (zobrist-hash-board board)
                                 (zobrist-hash-player player))))
  (let ((state (make-instance 'state)))
    (with-slots ((b board) (p player) (h hash)) state
      (setf b board
            p player
            h hash))
    state))

(defun make-initial-state ()
  (make-state))

(defun copy-state (state)
  (make-state (copy-board (state-board state))
              (state-player state)
              (state-hash state)))

(defun state-equal (a b)
  (and (eq (state-player a) (state-player b))
       (= (state-hash a) (state-hash b))
       (board-equal (state-board a) (state-board b))))
