(in-package :heks)

(declaim (optimize (debug 3) (safety 3)))

(defparameter *exp-ceiling* (log most-positive-single-float))
(declaim (single-float *exp-ceiling*))

(declaim (inline tansig))
(defun tansig (x)
  (let ((kak (* -2 x)))
    (cond ((>= kak *exp-ceiling*) -1.0)
          (t (- (/ 2.0 (+ 1.0 (exp kak))) 1.0)))))

;; element-wise sigmoid, x is modified
(defun vector-tansig! (x)
  (declare (optimize (speed 3) (safety 1))
           (type (vector single-float) x))
  (do ((i 0 (1+ i))
       (m (length x)))
      ((>= i m))
    (setf (aref x i) (tansig (aref x i))))
  x)

(defun net-load-arrays (kind)
  (iter (for i from 1 to 4)
        (collecting
          (load-array-from-file (format nil "/home/tim/school/isg/3rbms_finetuned_log/~A~A" (string kind) i)))))

(defparameter *net-weights* (net-load-arrays "W"))
(defparameter *net-biases*  (mapcar #'array-as-vector (net-load-arrays "b")))
(defparameter *net-activations*
  (list #'vector-tansig! #'vector-tansig! #'vector-tansig! #'identity))

(defun net-transfer (x w b f)
  (declare (optimize (speed 3) (safety 1))
           (type (array single-float) x w b)
           (function f))
  (destructuring-bind (m n) (array-dimensions w)
    (declare (array-index m n))
    (assert (= (length x) m))
    (do ((y (make-array n :element-type 'single-float :initial-element 0.0))
         (j 0 (1+ j)))
        ((>= j n) (funcall f y))
      (setf (aref y j)
            (do ((dot 0.0)
                 (i 0 (1+ i)))
                ((>= i m) (+ dot (aref b j)))
              (incf dot (* (aref x i) (aref w i j))))))))

(declaim (ftype (function (state) (single-float 0.0 1.0)) net-evaluate-state*))
(defun net-evaluate-state* (state)
  (iter (with (the (array single-float) x) = (state->vector state))
        (for (the (array single-float) w) in *net-weights*)
        (for (the (array single-float) b) in *net-biases*)
        (for f in *net-activations*)
        (setf x (net-transfer x w b f))
        (finally
         (assert (= 1 (length x)))
         (return (exp (min 0 (aref x 0)))))))

(defun net-evaluate-state (data state moves)
  (declare (optimize (speed 3) (safety 1))
           (ignore data moves))
  (round (* *evaluation-granularity* (net-evaluate-state* state))))

(defparameter *net-evaluator*
  (make-evaluator :getter #'net-evaluate-state))

