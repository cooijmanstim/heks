(in-package :heks)

(declaim (optimize (debug 3) (safety 3)))

(defparameter *killer-count* 3)
(defparameter *killer-table-ply-count* 50)

(defun make-killer-table ()
  (make-array (list *killer-table-ply-count*) :initial-element '()))

(defun lookup-killers (table ply valid-moves)
  (when (< ply *killer-table-ply-count*)
    (intersection (aref table ply) valid-moves)))

(defun store-killer (table ply move)
  (symbol-macrolet ((killers (aref table ply)))
    (deletef killers move :test #'move-equal)
    ;; hack around a bit to keep a constant-length list
    (push move killers)
    (when-let ((max-cdr (nthcdr (1- *killer-count*) killers)))
      (rplacd max-cdr nil))))

;; move killers one or more plies forward
(defun update-killer-table (table)
  (rotate table -1))

;; move killers one or more plies backward
(defun downdate-killer-table (table)
  (rotate table 1))
