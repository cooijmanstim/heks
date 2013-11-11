(in-package :heks)

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
    `(let* (,@(mapcar #'list names vals)
            (,(car newval) (nbutlast ,getter))
            ,@(cdr newval))
       ,setter)))      

; TODO: use array rather than cons?
(defun v (x y)
  (cons x y))

(defun s1 (v)
  (car v))

(defun s2 (v)
  (cdr v))

(defun v+v (u v)
  (cons (+ (s1 u) (s1 v))
        (+ (s2 u) (s2 v))))

(defun s+v (s v)
  (cons (+ s (s1 v))
        (+ s (s2 v))))

(defun s*v (s v)
  (cons (* s (s1 v))
        (* s (s2 v))))

(defun vmap (fn v)
  (list->v (mapcar fn (v->list v))))

(defun v-v (u v)
  (v (- (s1 u) (s1 v))
     (- (s2 u) (s2 v))))

(defun v.v (u v)
  (+ (* (s1 u) (s1 v)) (* (s2 u) (s2 v))))

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

