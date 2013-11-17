(in-package :heks)

(declaim (optimize (debug 3)))

(defparameter *zobrist-ceiling* (1+ most-positive-fixnum))
(defun zobrist-bitstring ()
  (random *zobrist-ceiling*))

(defparameter *zobrist-bitstrings*
  (let ((positions (make-array (list (- *board-size* 1) (- *board-size* 1) 2 2)
                               :element-type 'fixnum
                               :adjustable nil))
        (black-to-move (zobrist-bitstring)))
    (iter (for i from 1 below (- *board-size* 1))
          (iter (for j from 1 below (- *board-size* 1))
                (iter (for player in '(:white :black))
                      (for k from 0)
                      (iter (for object in '(:man :king))
                            (for l from 0)
                            (setf (aref positions i j k l) (zobrist-bitstring))))))
    (list positions black-to-move)))

;; desperately-TODO: stop using keywords for white/black and man/king
(defun tile-zobrist-bitstring (tile ij)
  (aref (first *zobrist-bitstrings*)
        (s1 ij) (s2 ij)
        (ccase (tile-owner tile)
          (:white 0)
          (:black 1))
        (ccase (tile-object tile)
          (:man 0)
          (:king 1))))
(defun zobrist-player-bitstring ()
  (second *zobrist-bitstrings*))

(define-modify-macro logxorf (&rest numbers) logxor)

(defun zobrist-hash-board (board)
  (let ((hash 0)
        (zobrist-bitstrings (first *zobrist-bitstrings*)))
    (iter (for i from 1 below (- *board-size* 1))
          (iter (for j from 1 below (- *board-size* 1))
                (for tile = (board-tile board (v i j)))
                (for k = (case (tile-owner tile)
                           (:white 0)
                           (:black 1)
                           (otherwise nil)))
                (for l = (case (tile-object (board-tile board (v i j)))
                           (:man 0)
                           (:king 1)
                           (otherwise nil)))
                (unless (or (null k) (null l))
                  (logxorf hash (aref zobrist-bitstrings i j k l)))))
    hash))

(defun zobrist-hash-player (player)
  (if (eq player :black)
      (second *zobrist-bitstrings*)
      0))

(defun zobrist-hash (state)
  (logxor (zobrist-hash-board (state-board state))
          (zobrist-hash-player (state-player state))))

