(in-package :heks)

(declaim (optimize (debug 3)))

(setq lisp-unit:*print-failures* t)
(lisp-unit:use-debugger)

(defun make-test-board-with (ijts)
  (let ((board (make-empty-board)))
    (iter (for ijt in ijts)
          (destructuring-bind (i j object owner) ijt
            (setf (aref board i j) (make-tile object owner))))
    board))

;; allows us to be lazy and specify points as lists and therefore movesets as
;; simple quoted lists.
(defun designated-move (move-designator)
  (mapcar #'designated-v move-designator))

(defun designated-v (v-designator)
  (if (consp v-designator)
      (if (consp (cdr v-designator))
          (apply #'v v-designator)
          v-designator)
      ;; assume sequence
      (v (elt v-designator 0)
         (elt v-designator 1))))

(defmacro assert-moveset (expected form &optional extras)
  `(assert-equality #'moveset-equal
                    (mapcar #'designated-move ,expected)
                    ,form
                    ,@extras))

(define-test v
  (assert-equality #'v= (v 0 0) (vmap (constantly 0) (v 1 2)))
  (assert-equality #'tree-equal '(1 2) (v->list (make-v 1 2))))

(define-test board-screen-transform
  (iter (repeat 5)
        (for ij = (v (random 10) (random 10)))
        (assert-equality #'v= ij (board-position (window-position ij)))))

(define-test displacement-direction
  (iter (repeat 2)
        (for ij = (v (random 10) (random 10)))
        (iter (for direction in *all-directions*)
              (iter (for distance from 1 to 3)
                    (multiple-value-bind (direction2 distance2)
                        (displacement-direction ij (v+v ij (s*v distance direction)))
                      (assert-equal direction direction2)
                      (assert-equal distance distance2))))))

(define-test piece-moves
  (let ((board (make-test-board-with '((1 1 :man :white)
                                       (1 2 :man :black)
                                       (2 1 :man :black)
                                       (2 3 :man :black)
                                       (3 4 :man :white)))))
    (assert-moveset '(((1 1) (2 2)))
                    (piece-moves board (v 1 1)))
    (assert-moveset '(((3 4) (3 5))
                      ((3 4) (4 4))
                      ((3 4) (4 5)))
                    (piece-moves board (v 3 4)))
    (assert-moveset '()
                    (piece-moves board (v 1 2)))
    (assert-moveset '()
                    (piece-moves board (v 2 1)))
    (assert-moveset '(((2 3) (1 3))
                      ((2 3) (2 2)))
                    (piece-moves board (v 2 3)))))

(define-test piece-captures
  (let ((board (make-test-board-with '((1 1 :man :white)
                                       (1 2 :man :black)
                                       (2 1 :man :black)
                                       (2 3 :man :black)
                                       (3 4 :man :white)))))
    (assert-moveset '(((1 1) (3 1))
                      ((1 1) (1 3))
                      ((1 1) (1 3) (3 3)))
                    (piece-captures board (v 1 1)))
    (assert-moveset '()
                    (piece-captures board (v 3 4)))
    (assert-moveset '()
                    (piece-captures board (v 1 2)))
    (assert-moveset '()
                    (piece-captures board (v 2 1)))
    (assert-moveset '(((2 3) (4 5)))
                    (piece-captures board (v 2 3)))))

(define-test moves
  (let ((board (make-test-board-with '((1 1 :man :white)
                                       (1 2 :man :black)
                                       (2 1 :man :black)
                                       (2 3 :man :black)
                                       (3 4 :man :white)))))
    (assert-moveset '(((1 1) (1 3) (3 3)))
                    (moves (make-state :board board :player :white)))
    (assert-moveset '(((2 3) (4 5)))
                    (moves (make-state :board board :player :black)))))

