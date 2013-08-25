;;; -*- Mode: Lisp -*-

(defpackage :ouat
  (:use :cl :asdf))

(in-package :ouat)

(require :clx)
(require :sb-posix)
(require :vecto)

(defsystem :ouat
    :name "olaugh's unhelpful anagramming tools"
    :author "John O'Laughlin <olaughlin@gmail.com>"
    :version "0.00"
    :maintainer "John O'Laughlin <olaughlin@gmail.com>"
    :description "various wordgame# utilities"
    :serial t
    :components ((:file "package")
                 (:file "macros")
                 (:file "util")
                 (:file "dawg")
                 (:file "shell")
                 (:file "examples")
                 (:file "scrabble-util")
                 (:file "scrabble-structs")
                 (:file "scrabble-constants")
                 (:file "scrabble-debug")
		 (:file "scrabble-move-finding")
                 (:file "scrabble-test")
                 (:file "tile-graphics")))
