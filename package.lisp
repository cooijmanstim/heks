(defpackage :heks
  (:use :cl :alexandria :iterate)
  (:import-from :cl-utilities :extrema)
  (:import-from :lisp-unit
                :define-test :run-tests
                :assert-true :assert-equal :assert-equality)
  (:export :main))
