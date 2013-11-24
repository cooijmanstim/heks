(in-package :heks)

(declaim (optimize (debug 3)))

(setq lisp-unit:*print-failures* t)
(lisp-unit:use-debugger)

(defun make-test-board-with (ijts)
  (let ((board (make-empty-board)))
    (iter (for ijt in ijts)
          (destructuring-bind (i j object owner) ijt
            (setf (board-tile board (v i j)) (make-tile object owner))))
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
  (assert-true (v= (v 1 1) (v 1 1)))
  (assert-equality #'v= (v 0 0) (vmap (constantly 0) (v 1 2)))
  (assert-equality #'tree-equal '(1 2) (v->list (v 1 2))))

(define-test move-equal
  (assert-false (move-equal (list (v 1 1) (v 1 3))
                            (list (v 1 1) (v 1 3) (v 3 3)))))

(define-test far-modify
  (let ((l (list 1 2 3 4 5))
        (x 6))
    (iter (for k from 0 to (length l))
          (multiple-value-bind (l1 l2)
              (nbutlast* (copy-list l) k)
            (assert-equal l (append l1 l2))))
    (multiple-value-bind (l1 l2)
        (nbutlast* (copy-list l) (length l))
      (assert-equal '() l1)
      (assert-equal l l2))
    (multiple-value-bind (l3 l4)
        (nbutlast* (copy-list l) (+ 1 (length l)))
      (assert-equal '() l3)
      (assert-equal l l4))
    (let ((l (copy-list l)))
      (multiple-value-bind (l5 l6)
          (nbutlast* l 2)
        (assert-equal '(1 2 3) l5)
        (assert-equal '(4 5) l6)
        (assert-equal '(1 2 3) l)))
    (let ((l (copy-list l)))
      (far-push x l)
      (assert-equal x (lastcar l))
      (assert-equal x (far-pop l))
      (assert-equal 5 (lastcar l))))
  (let ((l (list 1 2)))
    (far-pop l)
    (assert-equal '(1) l)))

;; only makes sense if using a linear-array representation for board
'(define-test board-rmi->ij
  (iter (repeat 5)
        (with board = (make-initial-board))
        (for ij = (vmap #'random *board-dimensions*))
        (assert-equality #'v= ij (board-rmi->ij board (linear-array-index board (s1 ij) (s2 ij))))))

(define-test linear-array-transform
  (let ((larray (make-linear-array '(2 2)))
        (indices '(0 1 2 3))
        (subscriptss '((0 0) (0 1) (1 0) (1 1))))
    (assert-equal 4 (linear-array-size larray))
    (iter (for index in indices)
          (for subscripts in subscriptss)
          (assert-equal index (apply #'linear-array-index larray subscripts))
          (assert-equal subscripts (linear-array-subscripts larray index))
          (assert-equal index (apply #'linear-array-index larray (linear-array-subscripts larray index)))
          (assert-equal subscripts (linear-array-subscripts larray (apply #'linear-array-index larray subscripts))))))

(define-test board-screen-transform
  (iter (repeat 5)
        (for ij = (vmap #'random *board-dimensions*))
        (assert-equality #'v= ij (board-position (window-position ij))))
  (assert-equality #'v= *window-center* (window-position *board-center*)))

(define-test tile-equal
  (iter (repeat 5)
        (for ij = (vmap #'random *board-dimensions*))
        (assert-equality #'tile-equal (initial-tile ij) (initial-tile ij))))

(define-test board-equal
  (assert-equality #'board-equal (make-initial-board) (make-initial-board)))

(define-test copy-board
  (assert-equality #'board-equal (make-initial-board) (copy-board (make-initial-board))))

(define-test displacement-direction
  (iter (repeat 2)
        (for ij = (v (random 10) (random 10)))
        (iter (for direction in *all-directions*)
              (iter (for distance from 1 to 3)
                    (multiple-value-bind (direction2 distance2)
                        (displacement-direction ij (v+v ij (s*v distance direction)))
                      (assert-equal direction direction2)
                      (assert-equal distance distance2))))))

(define-test submove
  (assert-true (submovep '((1 . 1)) '((1 . 1) (1 . 2))))
  (assert-false (submovep '((1 . 1)) '((1 . 2) (1 . 3)))))

(define-test supermoves
  (let ((moves '(((1 . 1) (2 . 2))
                 ((1 . 1) (2 . 1))
                 ((1 . 2) (2 . 3)))))
    (assert-moveset '(((1 1) (2 2))
                      ((1 1) (2 1)))
                    (supermoves '((1 . 1)) moves))
    (assert-moveset '()
                    (supermoves '((3 . 3)) moves))
    (assert-moveset '(((1 2) (2 3)))
                    (supermoves '((1 . 2)) moves))))

(define-test iter-board
  (let* ((board (make-empty-board))
         (ijs (iter (for tile at ij of board)
                    (collect ij)))
         (ijs-black (iter (for tile at ij of board from :black)
                          (collect ij))))
    (assert-equal 61 (length ijs))
    (assert-equal (v 1 1) (first ijs))
    (assert-equal (s+v -2 *board-dimensions*) (lastcar ijs))
    (assert-equal ijs (reverse ijs-black))))

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
                    (moves (make-state board :white)))
    (assert-moveset '(((2 3) (4 5)))
                    (moves (make-state board :black)))))

(define-test remove-piece
  (let* ((state (make-state (make-test-board-with '((1 1 :man :white))))))
    (remove-piece state (v 1 1))
    (assert-equal :empty (tile-object (board-tile (state-board state) (v 1 1))))))

(define-test displacement-direction
  (multiple-value-bind (didj n)
      (displacement-direction (v 1 1) (v 1 2))
    (assert-equal (v 0 1) didj)
    (assert-equal 1 n))
  (multiple-value-bind (didj n)
      (displacement-direction (v 1 1) (v 2 2))
    (assert-equal (v 1 1) didj)
    (assert-equal 1 n)))

(define-test apply-move
  (let* ((test-board-designator '((1 1 :man :white)
                                  (1 2 :man :black)
                                  (2 1 :man :black)
                                  (2 3 :man :black)
                                  (3 4 :man :white)))
         (state (make-state (make-test-board-with test-board-designator) :white))
         (breadcrumbs '()))
    (push (apply-move state (designated-move '((1 1) (1 3) (3 3)))) breadcrumbs)
    (assert-equal :empty (tile-object (board-tile (state-board state) (v 1 2))))
    (assert-equal :empty (tile-object (board-tile (state-board state) (v 2 3))))
    (unapply-move state (pop breadcrumbs))
    (assert-equal :man (tile-object (board-tile (state-board state) (v 1 2))))
    (assert-equal :man (tile-object (board-tile (state-board state) (v 2 3))))
    (toggle-player state)
    (push (apply-move state (designated-move '((2 3) (4 5)))) breadcrumbs)
    (assert-equal :empty (tile-object (board-tile (state-board state) (v 3 4))))
    (unapply-move state (pop breadcrumbs))
    (assert-equal :man (tile-object (board-tile (state-board state) (v 3 4))))))

;; check hash consistency after applying and unapplying 20 moves
(define-test hash
  (iter (repeat 10)
        (let ((state (make-initial-state))
              (breadcrumbs '()))
          (iter (repeat 20)
                (for moves = (moves state))
                (push (apply-move state (random-elt moves)) breadcrumbs)
                (assert-equal (zobrist-hash state) (state-hash state)))
          (assert-false (= (state-hash state) 0))  ; duh
          (iter (for breadcrumb in breadcrumbs)
                (unapply-move state breadcrumb)
                (assert-equal (zobrist-hash state) (state-hash state))))))

;; commented out because verbose
'(define-test spsa
  (labels ((f (x)
             ;; a convex quadratic function with minimum at 0
             (let ((H #2A((5 4 3)
                          (4 4 2)
                          (3 2 5))))
               (iter (for i from 0 to 2)
                     (summing (iter (for j from 0 to 2)
                                    (summing (* (aref x j) (aref H i j) (aref x i)))))))))
    (assert-float-equal 0.0 (f #(0.0 0.0 0.0)))
    (multiple-value-bind (x* xs)
        (spsa 1000 #'f (vector (random 10) (random 10) (random 10)) :c 0.1)
      (let ((fs (mapcar #'f xs)))
        (assert-true (every (lambda (x)
                              (< (abs x) 1e-3))
                            x*)
                     x* xs fs)))))

