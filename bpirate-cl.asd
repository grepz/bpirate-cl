;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(asdf:defsystem bpirate-cl
  :version "0"
  :description "BusPirate v4 binary interface"
  :maintainer "Stanislav M. Ivankin <lessgrep@gmail.com>"
  :author "Stanislav M. Ivankin <lessgrep@gmail.com>"
  :licence "GPLv2"
  :depends-on (:sb-posix :flexi-streams)
  :serial t
  ;; components likely need manual reordering
  :components ((:module "src"
			:serial t
			:components ((:file "package")
				     (:file "serial")
				     (:file "utils")
				     (:file "bpirate-mode")
				     (:file "bpirate-test")
				     (:file "bpirate-pwm")
				     (:file "bpirate-uart")
				     (:file "bpirate"))))
  ;; :long-description ""
  )
