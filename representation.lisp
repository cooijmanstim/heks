(in-package :heks)

(declaim (optimize (debug 3)))


(deftype player ()
  '(member :white :black))

;; TODO: low-hanging optimization fruit here
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


; XXX: it is assumed that owner is non-nil iff object is :man or :king
(defstruct (tile (:constructor make-tile (object &optional owner)) (:copier nil))
  (object :void :type (member :void     ; the tile is not part of the board (necessary
                                        ; because we're cramming a hexagonal board into
                                        ; a rectangular array
                              :empty    ; there is nothing on the tile
                              :man :king))
  (owner nil :type (or nil player)))

(defun copy-tile (tile)
  (make-tile (tile-object tile) (tile-owner tile)))

(defun tile-equal (a b)
  (and (eq (tile-object a) (tile-object b))
       (eq (tile-owner  a) (tile-owner  b))))

(defun tile-empty-p (board ij)
  (eq (tile-object (board-tile board ij)) :empty))

; since our choice of representation already forces us to deal with :void tiles,
; we might as well use a border of :void tiles to avoid the need for explicit
; bounds checking.  hence 11 instead of 9 here.
(deftype board ()
  '(simple-array tile (11 11)))

(defparameter *board-size* 11)
(declaim (fixnum *board-size*))
(defparameter *board-dimensions* (v *board-size* *board-size*))
(defparameter *board-center* (scale-vector 1/2 (s+v -1 *board-dimensions*)))

(declaim (inline board-tile set-board-tile))
(defun board-tile (board ij)
  (declare (optimize (speed 3))
           (type board board))
  (aref board (s1 ij) (s2 ij)))
(defun set-board-tile (board ij tile)
  (declare (optimize (speed 3))
           (type board board)
           (type tile tile))
  (setf (aref board (s1 ij) (s2 ij)) tile))
(defsetf board-tile set-board-tile)

;; iter clause for iterating over board interior
;; by default, iterates in the order a1, b1, ..., i1, b2, ... etc.  :void tiles are
;; skipped.
;; use FROM :black to reverse the order of iteration
(defmacro-driver (FOR tilevar AT ijvar OF boardexpr &optional FROM playerexpr)
  (let ((keyword (if generate 'generate 'for)))
    (with-gensyms (playervar boardvar rmivar maxrmivar)
      `(progn
         (with ,playervar = ,playerexpr)
         (with ,boardvar = ,boardexpr)
         (with ,maxrmivar = (array-total-size ,boardvar))
         (with ,rmivar = 0)
         (declare (fixnum ,rmivar ,maxrmivar))
         (,keyword ,ijvar
                   next (progn
                          (incf ,rmivar)
                          (when (>= ,rmivar ,maxrmivar)
                            (terminate))
                          (list->v
                           (array-index-row-major ,boardvar
                                                  (if (eq ,playervar :black)
                                                      (- ,maxrmivar ,rmivar 1)
                                                      ,rmivar)))))
         (,keyword ,tilevar next (board-tile ,boardvar ,ijvar))
         (when (eq (tile-object ,tilevar) :void)
           (next-iteration))))))

(defun initial-tile (ij)
  (let* ((board-interior-dimensions (s+v -2 *board-dimensions*))
         (ij (s+v -1 ij))
         ;; cumulative inverse coordinates
         (kl (s+v -1 (v-v board-interior-dimensions ij))))
    (cond ((or (< (s1 ij) 0) (< (s2 ij) 0)
               (< (s1 kl) 0) (< (s2 kl) 0)
               (<= (+ (s2 kl) (s1 ij)) 3)
               (<= (+ (s1 kl) (s2 ij)) 3)) (make-tile :void))
          ((absv<=s ij 3) (make-tile :man :white))
          ((absv<=s kl 3) (make-tile :man :black))
          (t (make-tile :empty)))))

(defun crown-p (tile ij)
  (with-slots (object owner) tile
    (and (eq object :man)
         (ccase owner
           (:black (or (= (s1 ij) 1) (= (s2 ij) 1)))
           (:white (or (= (s1 ij) (- *board-size* 2))
                       (= (s2 ij) (- *board-size* 2))))))))


(defun make-initial-board ()
  (let ((board (make-array (v->list *board-dimensions*)
                           :element-type 'tile
                           :initial-element (make-tile :void))))
    (iter (for i from 1 below (- (s1 *board-dimensions*) 1))
          (iter (for j from 1 below (- (s2 *board-dimensions*) 1))
                (for ij = (v i j))
                (setf (board-tile board ij) (initial-tile ij))))
    board))

(defun make-empty-board ()
  (let ((board (make-initial-board)))
    (iter (for tile at ij of board)
          (setf (board-tile board ij) (make-tile :empty)))
    board))

(defun copy-board (board)
  (let ((board2 (make-array (array-dimensions board)
                            :element-type 'tile)))
    (iter (for i from 0 below (s1 *board-dimensions*))
          (iter (for j from 0 below (s2 *board-dimensions*))
                (for ij = (v i j))
                (setf (board-tile board2 ij) (copy-tile (board-tile board ij)))))
    board2))

(defun board-equal (board1 board2)
  (iter (for i from 0 below (s1 *board-dimensions*))
        (always
         (iter (for j from 0 below (s2 *board-dimensions*))
               (for ij = (v i j))
               (always (tile-equal (board-tile board1 ij)
                                   (board-tile board2 ij)))))))

(defstruct (state (:constructor nil) (:copier nil))
  (board nil :type (or null board))
  (player :white :type player)
  (endp nil :type boolean)
  (hash 0 :type zobrist-hash))

(defun make-state (&optional
                   (board (make-initial-board))
                   (player :white)
                   (hash (logxor (zobrist-hash-board board)
                                 (zobrist-hash-player player)))
                   (endp nil))
  (let ((state (make-instance 'state)))
    (with-slots ((b board) (p player) (h hash) (e endp)) state
      (setf b board
            p player
            h hash
            e endp))
    state))

(defun make-initial-state ()
  (make-state))

(defun copy-state (state)
  (with-slots (board player hash endp) state
    (make-state (copy-board board) player hash endp)))

(defun state-equal (a b)
  (and (eq (state-player a) (state-player b))
       (= (state-hash a) (state-hash b))
       (board-equal (state-board a) (state-board b))))

(defun state-winner (state)
  (with-slots (endp player) state
    (assert endp)
    (opponent player)))
