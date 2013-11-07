(defpackage heks-system
  (:use :common-lisp :asdf))

(in-package heks-system)

(defsystem heks
  :description "HexDame implementation with AI player"
  :long-description
  "FIXME"
  :version "0.0"
  :author "Tim Cooijmans <t.cooijmans@student.maastrichtuniversity.nl>"
  :maintainer "Tim Cooijmans <t.cooijmans@student.maastrichtuniversity.nl>"
  :licence "GPL"
  :depends-on (:alexandria :lisp-unit :cl-utilities :iterate)
  :serial t
  :components
  ((:file "package")
   (:file "heks")
   (:file "test")))

(require :sb-sprof)