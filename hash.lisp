(in-package :heks)

(declaim (optimize (debug 3)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *zobrist-bitstring-length* 128))
(deftype zobrist-hash () `(unsigned-byte ,*zobrist-bitstring-length*))

(defun zobrist-bitstring ()
  (random (expt 2 *zobrist-bitstring-length*)))

(defparameter *zobrist-bitstring-positions*
  (let ((positions (make-array (append (v->list *board-dimensions*) (list 2 2))
                               :element-type 'zobrist-hash :initial-element 0))
        (board (make-initial-board))) ;; to make looping easier
    (iter (for tile at ij of board)
          (iter (for k in (list *white-player* *black-player*))
                (iter (for object in '(:man :king))
                      (for l from 0)
                      (setf (aref positions (s1 ij) (s2 ij) k l) (zobrist-bitstring)))))
    positions))
(defparameter *zobrist-bitstring-black-to-move* (zobrist-bitstring))
(declaim (type (simple-array zobrist-hash) *zobrist-bitstring-positions*)
         (zobrist-hash *zobrist-bitstring-black-to-move*))

(declaim (ftype (function (tile v) zobrist-hash) tile-zobrist-bitstring))
(defun tile-zobrist-bitstring (tile ij)
  (declare (optimize (speed 3) (safety 1))
           (tile tile)
           (v ij))
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
