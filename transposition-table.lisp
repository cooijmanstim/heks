(in-package :heks)

(declaim (optimize speed space))

(defstruct transposition
  (hash 0 :type zobrist-hash)
  (depth 0 :type fixnum)
  (value 0 :type evaluation)
  (type nil :type (or null (member :exact :lower :upper)))
  (move '()))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *transposition-table-key-length* 24)
  (defparameter *transposition-table-key-ceiling* (expt 2 *transposition-table-key-length*)))

(defun make-transposition-table ()
  (make-array *transposition-table-key-ceiling*
              :element-type 'transposition
              :initial-element (make-transposition)))

(defun transposition-table-key (state)
  (multiple-value-bind (quotient remainder)
      (truncate (state-hash state) *transposition-table-key-ceiling*)
    (declare (ignore quotient))
    remainder))

(defun lookup-transposition (table state &optional (depth 0))
  (declare (optimize speed)
           (fixnum depth))
  (let ((transposition (svref table (transposition-table-key state))))
    (when (and transposition
               (= (transposition-hash transposition) (state-hash state))
               (>= (transposition-depth transposition) depth))
      transposition)))

(defun ensure-transposition (table state)
  (if-let ((transposition (lookup-transposition table state)))
    transposition
    (setf (svref table (transposition-table-key state))
          (make-transposition :hash (state-hash state)))))
