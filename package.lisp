(defpackage :heks
  (:use :cl :alexandria :iterate)
  (:shadowing-import-from :iterate :finish)
  (:import-from :cl-utilities :extrema)
  (:import-from :lisp-unit
                :define-test :run-tests
                :assert-true :assert-false
                :assert-equal :assert-equalp :assert-equality
                :assert-float-equal)
  (:export :main))
