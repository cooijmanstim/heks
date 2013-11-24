(in-package :heks)

(declaim (optimize (debug 3)))

;; multi-dimensional array with explicitly linear storage
(defstruct (linear-array (:constructor nil)
                         (:copier nil))
  (dimensions '(0) :type list)
  (vector #() :type simple-vector))

(defun make-linear-array (dimensions &rest make-array-keyword-arguments)
  (let ((linear-array (make-instance 'linear-array)))
    (with-slots ((d dimensions) (v vector)) linear-array
      (setf d dimensions)
      (setf v (apply #'make-array (reduce #'* dimensions)
                     :adjustable nil :fill-pointer nil
                     make-array-keyword-arguments)))
    linear-array))

(defun linear-array-size (linear-array)
  (length (linear-array-vector linear-array)))

(defun linear-array-index (linear-array &rest subscripts)
  (assert (length= (linear-array-dimensions linear-array) subscripts))
  (do ((ns (rest (linear-array-dimensions linear-array)) (rest ns))
       (is subscripts (rest is))
       (index 0))
      ((null ns) (+ index (car is)))
    (incf index (reduce #'* ns :initial-value (car is)))))

(defun linear-array-subscripts (linear-array index)
  (do ((subs (list index) (cons q (rplaca subs r)))
       (dims (reverse (cdr (linear-array-dimensions linear-array))) (cdr dims))
       q r)
      ((null dims) subs)
    (multiple-value-setq (q r) (truncate (car subs) (car dims)))))

(defun linear-aref (linear-array &rest subscripts)
    (svref (linear-array-vector linear-array)
           (apply #'linear-array-index linear-array subscripts)))

(define-setf-expander linear-aref (linear-array &rest subscripts &environment env)
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion linear-array env)
    (declare (ignore newval setter))
      (with-unique-names (storevar larrayvar subscriptsvar)
        (values (append (list larrayvar subscriptsvar)        dummies)
                (append (list getter    `(list ,@subscripts)) vals)
                `(,storevar)
                `(setf (svref (linear-array-vector ,larrayvar)
                              (apply #'linear-array-index ,larrayvar ,subscriptsvar))
                       ,storevar)
                `(linear-aref ,getter ,subscriptsvar)))))

;; linear-aref but with linear index
(defun linear-vref (linear-array index)
  (svref (linear-array-vector linear-array) index))

(defun set-linear-vref (linear-array index value)
  (setf (svref (linear-array-vector linear-array) index) value))

(defsetf linear-vref set-linear-vref)

(defclause-sequence in-linear-array index-of-linear-array
  :access-fn 'linear-vref
  :size-fn 'linear-array-size)
