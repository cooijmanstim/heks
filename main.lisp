(asdf:compile-system :heks)
(asdf:load-system :heks)

(in-package :heks)

(defun main ()
  (let ((board (make-initial-board)))
    (fresh-line)
    (print (array-dimensions board))
    (fresh-line)
    (print board)
    (fresh-line)
    (format-board t board)))

(main)
