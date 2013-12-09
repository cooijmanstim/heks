(in-package :heks)

(declaim (optimize (debug 3)))

(deftype zobrist-hash () 'fixnum)

(defparameter *zobrist-ceiling* most-positive-fixnum)
(defun zobrist-bitstring ()
  (random *zobrist-ceiling*))

(defparameter *zobrist-bitstring-positions*
  (let ((positions (make-array (append (v->list *board-dimensions*) (list 2 2))
                               :element-type 'fixnum :initial-element 0))
        (board (make-initial-board))) ;; to make looping easier
    (iter (for tile at ij of board)
          (iter (for k in (list *white-player* *black-player*))
                (iter (for object in '(:man :king))
                      (for l from 0)
                      (setf (aref positions (s1 ij) (s2 ij) k l) (zobrist-bitstring)))))
    positions))
(defparameter *zobrist-bitstring-black-to-move* (zobrist-bitstring))

;; desperately-TODO: stop using keywords for white/black and man/king
(declaim (ftype (function (tile v) zobrist-hash) tile-zobrist-bitstring))
(defun tile-zobrist-bitstring (tile ij)
  (with-slots (object owner) tile
    (if (not (member object '(:man :king)))
        0
        (aref *zobrist-bitstring-positions*
              (s1 ij) (s2 ij)
              owner
              (ccase object
                (:man 0)
                (:king 1))))))

(defun zobrist-position-bitstring (tile ij)
  (with-slots (object owner) tile
    (if-let ((k owner)
             (l (case object
                  (:man 0)
                  (:king 1)
                (otherwise nil))))
      (aref *zobrist-bitstring-positions* (s1 ij) (s2 ij) k l)
      0)))

;; used by toggle-player*
(defun zobrist-player-bitstring ()
  *zobrist-bitstring-black-to-move*)

(defun zobrist-hash-board (board)
  (let ((hash 0))
    (iter (for tile at ij of board)
          (logxorf hash (zobrist-position-bitstring tile ij)))
    hash))

(defun zobrist-hash-player (player)
  (* player *zobrist-bitstring-black-to-move*))

(defun zobrist-hash (state)
  (with-slots (board player) state
    (logxor (zobrist-hash-board  board)
            (zobrist-hash-player player))))
