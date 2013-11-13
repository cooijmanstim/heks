(in-package :heks)

;; like nbutlast, but returning the second part of the list as an extra value
(defun nbutlast* (l &optional n)
  (assert (>= (length l) n))
  (iter (for x on l)
        (for x-ahead on (nthcdr (+ n 1) l))
        (finally
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

; TODO: use array rather than cons?
(defun v (x y)
  (cons x y))

(defun s1 (v)
  (car v))

(defun s2 (v)
  (cdr v))

(defun v+v (u v)
  (v (+ (s1 u) (s1 v))
     (+ (s2 u) (s2 v))))

(defun v-v (u v)
  (v (- (s1 u) (s1 v))
     (- (s2 u) (s2 v))))

(defun s+v (s v)
  (v (+ s (s1 v))
     (+ s (s2 v))))

(defun s*v (s v)
  (v (* s (s1 v))
     (* s (s2 v))))

(defun vmap (fn v)
  (list->v (mapcar fn (v->list v))))

(defun v.v (u v)
  (+ (* (s1 u) (s1 v))
     (* (s2 u) (s2 v))))

(defun v= (u v)
  (equalp u v))

(defun angle->v (a)
  (v (cos a) (sin a)))

(defun list->v (l)
  (v (first l) (second l)))

(defun v->list (v)
  (list (s1 v) (s2 v)))

(defun v->sdlpoint (v)
  (sdl:point :x (s1 v) :y (s2 v)))

(defun absv<=s (v s)
  (and (<= (s1 v) s)
       (<= (s2 v) s)))

