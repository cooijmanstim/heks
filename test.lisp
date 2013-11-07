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

(defun moveset-equal (a b)
  (set-equal a b :test #'tree-equal))

(define-test piece-moves
  (let ((board (make-test-board-with '((1 1 :man :white)
                                       (1 2 :man :black)
                                       (2 1 :man :black)
                                       (2 3 :man :black)
                                       (3 4 :man :white)))))
    (assert-equality #'moveset-equal
                     '(((1 . 1) (2 . 2)))
                     (piece-moves board :white 1 1))
    (assert-equality #'moveset-equal
                     '(((3 . 4) (3 . 5))
                       ((3 . 4) (4 . 4))
                       ((3 . 4) (4 . 5)))
                     (piece-moves board :white 3 4))
    (assert-equality #'moveset-equal
                     '()
                     (piece-moves board :black 1 2))
    (assert-equality #'moveset-equal
                     '()
                     (piece-moves board :black 2 1))
    (assert-equality #'moveset-equal
                     '(((2 . 3) (1 . 3))
                       ((2 . 3) (2 . 2)))
                     (piece-moves board :black 2 3))))

(define-test piece-captures
  (let ((board (make-test-board-with '((1 1 :man :white)
                                       (1 2 :man :black)
                                       (2 1 :man :black)
                                       (2 3 :man :black)
                                       (3 4 :man :white)))))
    (assert-equality #'moveset-equal
                     '(((1 . 1) (3 . 1))
                       ((1 . 1) (1 . 3))
                       ((1 . 1) (1 . 3) (3 . 3)))
                     (piece-captures board :white 1 1))
    (assert-equality #'moveset-equal
                     '()
                     (piece-captures board :white 3 4))
    (assert-equality #'moveset-equal
                     '()
                     (piece-captures board :black 1 2))
    (assert-equality #'moveset-equal
                     '()
                     (piece-captures board :black 2 1))
    (assert-equality #'moveset-equal
                     '(((2 . 3) (4 . 5)))
                     (piece-captures board :black 2 3))))

(define-test moves
  (let ((board (make-test-board-with '((1 1 :man :white)
                                       (1 2 :man :black)
                                       (2 1 :man :black)
                                       (2 3 :man :black)
                                       (3 4 :man :white)))))
    (assert-equality #'moveset-equal
                     '(((1 . 1) (1 . 3) (3 . 3)))
                     (moves (make-state :board board :player :white)))
    (assert-equality #'moveset-equal
                     '(((2 . 3) (4 . 5)))
                     (moves (make-state :board board :player :black)))))












