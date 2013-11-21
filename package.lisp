(defpackage :heks
  (:use :cl :alexandria :iterate)
  (:import-from :cl-utilities :extrema)
  (:import-from :lisp-unit
                :define-test :run-tests
                :assert-true :assert-false
                :assert-equal :assert-equality
                :assert-float-equal)
  (:export :main))
