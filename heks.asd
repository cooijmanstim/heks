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
  :depends-on (:alexandria :lisp-unit :cl-utilities :iterate
                           :cl-ppcre :lispbuilder-sdl :hh-redblack)
  :serial t
  :components
  ((:file "package")
   (:file "util")
   (:file "representation")
   (:file "hash")
   (:file "game")
   (:file "evaluation")
   (:file "neuralnet")
   (:file "ai")
   (:file "learning")
   (:file "agent")
   (:file "gui")
   (:file "test")
   (:file "heks")))

(require :sb-sprof)
