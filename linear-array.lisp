(in-package :heks)

(declaim (optimize (debug 3) (safety 3)))

;; multi-dimensional array with explicitly linear storage
(defstruct (linear-array (:constructor nil)
                         (:copier nil))
  (dimensions '(0) :type list)
  ;; linear index of subscripts (i j k ...) is the dot-product with this vector
  (coefficients (make-array 1 :element-type 'fixnum :initial-contents '(1)) :type (vector fixnum))
  (vector #() :type vector))

(defun make-linear-array (dimensions &rest make-array-keyword-arguments)
  (labels ((compute-coefficients (dimensions)
             (nconc (iter (for ds on (rest dimensions))
                          (collecting (reduce #'* ds)))
                    (list 1))))
    (let ((linear-array (make-instance 'linear-array)))
      (with-slots ((d dimensions) (c coefficients) (v vector)) linear-array
        (setf d dimensions)
        (setf c (make-array (length dimensions)
                            :adjustable nil
                            :element-type 'fixnum
                            :initial-contents (compute-coefficients dimensions)))
        (setf v (apply #'make-array (reduce #'* dimensions)
                       make-array-keyword-arguments)))
      linear-array)))

(defun linear-array-size (linear-array)
  (length (linear-array-vector linear-array)))

(declaim (ftype (function (linear-array &rest fixnum) fixnum) linear-array-index))
(defun linear-array-index (linear-array &rest subscripts)
  ;; (assert (length= (linear-array-dimensions linear-array) subscripts))
  (let ((coefficients (linear-array-coefficients linear-array)))
    (do ((is subscripts (rest is))
         (ci 0 (1+ ci))
         (index 0))
        ((null is) index)
      (declare (fixnum ci index)
               (optimize (speed 3) (safety 0)))
      (incf index (the fixnum (* (aref coefficients ci) (the fixnum (car is))))))))

(defun linear-array-subscripts (linear-array index)
  (do ((subs (list index) (cons q (rplaca subs r)))
       (dims (reverse (cdr (linear-array-dimensions linear-array))) (cdr dims))
       (q 0) (r 0))
      ((null dims) subs)
    (declare (fixnum q)
             (optimize (speed 3) (safety 0)))
    (multiple-value-setq (q r) (truncate (the fixnum (car subs))
                                         (the fixnum (car dims))))))

(defun linear-aref (linear-array &rest subscripts)
  (aref (linear-array-vector linear-array)
        (apply #'linear-array-index linear-array subscripts)))

(defun (setf linear-aref) (x linear-array &rest subscripts)
  (setf (aref (linear-array-vector linear-array)
              (apply #'linear-array-index linear-array subscripts))
        x))

;; linear-aref but with linear index
(defun linear-vref (linear-array index)
  (aref (linear-array-vector linear-array) index))

(defun set-linear-vref (linear-array index value)
  (setf (aref (linear-array-vector linear-array) index) value))

(defsetf linear-vref set-linear-vref)

(defclause-sequence in-linear-array index-of-linear-array
  :access-fn 'linear-vref
  :size-fn 'linear-array-size)
