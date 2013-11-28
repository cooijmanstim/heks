(in-package :heks)

;; like nbutlast, but returning the second part of the list as an extra value
(defun nbutlast* (l &optional n)
  (do ((x       (cons 'dummy l) (cdr x))
       (x-ahead (nthcdr n l)    (cdr x-ahead))
       (niters  0               (1+ niters)))
      ((null x-ahead)
       (when (zerop niters)
         (return (values '() l)))
       ;; x-ahead reaches the end, x reaches the cons where we need to make the cut
       (let ((last (cdr x)))
         (rplacd x nil)
         (return (values l last))))))

;; push/pop at end of list
(defmacro far-push (obj place &environment env)
  (multiple-value-bind (names vals newval setter getter)
      (get-setf-expansion place env)
    (let ((g (gensym)))
      `(let* ((,g ,obj)
              ,@(mapcar #'list names vals)
              (,(car newval) (nconc ,getter (list ,g)))
              ,@(cdr newval))
         ,setter))))

(defmacro far-pop (place &environment env)
  (multiple-value-bind (names vals newval setter getter)
      (get-setf-expansion place env)
    (let ((last-symbol  (gensym))
          (list-symbol (gensym)))
      `(let* (,@(mapcar #'list names vals))
         (multiple-value-bind (,list-symbol ,last-symbol)
             (nbutlast* ,getter 1)
           (let* ((,(car newval) ,list-symbol)
                  ,@(cdr newval))
             ,setter
             (first ,last-symbol)))))))

(defun no-op (&rest args)
  (declare (ignore args)))

;; inverse of array-row-major-index
;; taken from comp.lang.lisp
(defun array-index-row-major (array rmi)
  (do ((subs (list rmi) (cons q (rplaca subs r)))
       (dims (reverse (cdr (array-dimensions array))) (cdr dims))
       q r)
      ((null dims) subs)
    (multiple-value-setq (q r) (truncate (car subs) (car dims)))))

(define-modify-macro logxorf (&rest numbers) logxor)

;; an efficient 2d fixnum vector type
(deftype v ()
  '(simple-array fixnum (2)))

(declaim (inline v s1 s2 v+v v-v s+v s*v vmap v.v v= list->v v->list))
(defun v (x y)
  (declare (fixnum x y))
  (make-array 2 :element-type 'fixnum :initial-contents (list x y)))

(declaim (ftype (function (v) fixnum) s1 s2))
(defun s1 (v)
  (declare (v v))
  (aref v 0))
(defun s2 (v)
  (declare (v v))
  (aref v 1))

(declaim (ftype (function (v v) v) v+v v-v))
(defun v+v (u v)
  (v (the fixnum (+ (s1 u) (s1 v)))
     (the fixnum (+ (s2 u) (s2 v)))))

(defun v-v (u v)
  (v (the fixnum (- (s1 u) (s1 v)))
     (the fixnum (- (s2 u) (s2 v)))))

(defun v.v (u v)
  (+ (the fixnum (* (s1 u) (s1 v)))
     (the fixnum (* (s2 u) (s2 v)))))

(declaim (ftype (function (fixnum v) v) s+v s*v))
(defun s+v (s v)
  (v (the fixnum (+ s (s1 v)))
     (the fixnum (+ s (s2 v)))))

(defun s*v (s v)
  (v (the fixnum (* s (s1 v)))
     (the fixnum (* s (s2 v)))))

(defun vmap (fn v)
  (v (funcall fn (s1 v))
     (funcall fn (s2 v))))

(defun v= (u v)
  (equalp u v))

(defun list->v (l)
  (v (first l) (second l)))

(defun v->list (v)
  (list (s1 v) (s2 v)))

(defun absv<=s (v s)
  (and (<= (s1 v) s)
       (<= (s2 v) s)))

;; more general vector functionality for when efficiency doesn't matter
(defun vector-plus (u v &optional (result-type (type-of u)))
  (map result-type #'+ u v))

(defun vector-minus (u v &optional (result-type (type-of u)))
  (map result-type #'- u v))

(defun vector-times (u v &optional (result-type (type-of u)))
  (map result-type #'* u v))

(defun scale-vector (scalar vector &optional (result-type (type-of vector)))
  (map result-type (lambda (x) (* scalar x)) vector))

(defun vector-norm (vector &key (l 2))
  (expt (iter (for x in-vector vector)
              (sum (expt (abs x) l)))
        (/ 1 l)))

(defun dot-product (us vs)
  (assert (length= us vs))
  (iter (for u in-vector us)
        (for v in-vector vs)
        (summing (* u v))))

(defun matrix (initial-contents)
  (when-let ((m (length initial-contents))
             (n (length (elt initial-contents 0))))
    (make-array (list m n) :initial-contents initial-contents)))

(defun matrix-dot-vector (a x)
  (destructuring-bind (m n) (array-dimensions a)
    (let ((y (make-array m)))
      (iter (for i from 0 below m)
            (setf (aref y i)
                  (iter (for j from 0 below n)
                        (summing (* (aref a i j)
                                    (aref x j))))))
      y)))

(defun matrix-inverse (a)
  (assert (equalp '(2 2) (array-dimensions a))) ;; YAGNI
  (scale-matrix (/ 1 (- (* (aref a 0 0)
                           (aref a 1 1))
                        (* (aref a 0 1)
                           (aref a 1 0))))
                (matrix `((,   (aref a 1 1)  ,(- (aref a 0 1)))
                          (,(- (aref a 1 0))    ,(aref a 0 0))))))

(defun scale-matrix (scalar x)
  (destructuring-bind (m n) (array-dimensions x)
    (let ((y (make-array (list m n))))
      (iter (for i from 0 below m)
            (iter (for j from 0 below n)
                  (setf (aref y i j)
                        (* scalar (aref x i j)))))
      y)))

(defun angle->vector (a)
  (vector (cos a) (sin a)))

(defun vector->sdlpoint (vector)
  (sdl:point :x (elt vector 0)
             :y (elt vector 1)))

(defun transpose-lists (list-of-lists)
  (apply #'mapcar #'list list-of-lists))

;; fn must poll *out-of-time* and return when it becomes true
(defparameter *out-of-time* nil)
(defun time-limited (duration fn)
  (setq *out-of-time* nil)
  (sb-thread:make-thread
   (lambda ()
     (sleep duration)
     (setq *out-of-time* t)
     (sb-thread:thread-yield))
   :name "timer thread")
  (unwind-protect
       (funcall fn)
    (setq *out-of-time* nil)))

(defun profile (fn)
  (sb-sprof:reset)
  (sb-sprof:with-profiling (:loop nil)
    (funcall fn))
  (sb-sprof:report :type :flat))
