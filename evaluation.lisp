(in-package :heks)

(declaim (optimize (debug 3) (safety 3)))

(defparameter *evaluation-minimum* most-negative-fixnum)
(defparameter *evaluation-maximum* most-positive-fixnum)

(defun evaluation-inverse (v)
  (- v))

(defun piece-value (object)
  (case object
    (:man 1)
    (:king 3)
    (otherwise 0)))

(defun piece-differential (state)
  (let ((ours 0) (theirs 0))
    (declare (fixnum ours theirs))
    (iter (for tile at ij of (state-board state))
          (with-slots (object owner) tile
            (if (eq owner (state-player state))
                (incf ours (piece-value object))
                (incf theirs (piece-value object)))))
    (- ours theirs)))

(defun evaluate-state (state moves)
  (if (null moves)
      0 ;; no moves, player loses
      (+ (round (* 2 (gaussian-random)))
         (piece-differential state))))
