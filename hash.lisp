(in-package :heks)

(declaim (optimize (debug 3)))

(defparameter *zobrist-ceiling* most-positive-fixnum)
(defun zobrist-bitstring ()
  (random *zobrist-ceiling*))

(defparameter *zobrist-bitstring-positions*
  (let ((positions (make-linear-array (append (v->list *board-dimensions*) (list 2 2))
                                      :element-type 'fixnum :initial-element 0))
        (board (make-initial-board))) ;; to make looping easier
    (iter (for tile at ij of board)
          (iter (for player in '(:white :black))
                (for k from 0)
                (iter (for object in '(:man :king))
                      (for l from 0)
                      (setf (linear-aref positions (s1 ij) (s2 ij) k l) (zobrist-bitstring)))))
    positions))
(defparameter *zobrist-bitstring-black-to-move* (zobrist-bitstring))

;; desperately-TODO: stop using keywords for white/black and man/king
(defun tile-zobrist-bitstring (tile ij)
  (with-slots (object owner) tile
    (if (not (member object '(:man :king)))
        0
        (linear-aref *zobrist-bitstring-positions*
                     (s1 ij) (s2 ij)
                     (ccase owner
                       (:white 0)
                       (:black 1))
                     (ccase object
                       (:man 0)
                       (:king 1))))))

(defun zobrist-player-bitstring ()
  *zobrist-bitstring-black-to-move*)

(defun zobrist-position-bitstring (tile ij)
  (if-let ((k (case (tile-owner tile)
                (:white 0)
                (:black 1)
                (otherwise nil)))
           (l (case (tile-object tile)
                (:man 0)
                (:king 1)
                (otherwise nil))))
    (linear-aref *zobrist-bitstring-positions* (s1 ij) (s2 ij) k l)
    0))

(defun zobrist-hash-board (board)
  (let ((hash 0))
    (iter (for tile at ij of board)
          (logxorf hash (zobrist-position-bitstring tile ij)))
    hash))

(defun zobrist-hash-player (player)
  (if (eq player :black)
      *zobrist-bitstring-black-to-move*
      0))

(defun zobrist-hash (state)
  (logxor (zobrist-hash-board (state-board state))
          (zobrist-hash-player (state-player state))))

