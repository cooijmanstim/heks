(in-package :heks)

(declaim (optimize (debug 3) (safety 3)))

(defparameter *evaluation-maximum* (min (abs most-negative-fixnum) (abs most-positive-fixnum)))
(defparameter *evaluation-minimum* (- *evaluation-maximum*))

(defun evaluation-inverse (v)
  (- v))

(declaim (ftype (function (*) fixnum) piece-value piece-differential))

(defun piece-value (object)
  (case object
    (:man 1)
    (:king 3)
    (otherwise 0)))

(defun piece-differential (state)
  (let ((ours 0) (theirs 0))
    (declare (optimize (speed 3) (safety 0))
             (fixnum ours theirs))
    (with-slots ((us player)) state
      (iter (for tile at ij of (state-board state))
            (with-slots (object owner) tile
              (if (eq owner us)
                  (incf ours (piece-value object))
                  (incf theirs (piece-value object)))))
      (- ours theirs))))

(defun evaluate-state (state moves)
  (if (null moves)
      *evaluation-minimum* ;; no moves, player loses
      (+ (round (gaussian-random))
         (piece-differential state))))
