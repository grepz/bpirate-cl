;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:bpirate-cl)

(defclass bpirate-mode ()
  ((signature :initarg :signature)))

(defgeneric bpirate-mode-start (obj))

(defgeneric bpirate-mode-stop (obj))

(defgeneric bpirate-mode-reset (obj))

(defclass bpirate-uart-mode (bpirate-mode)
  ())

(defclass bpirate-spi-mode (bpirate-mode)
  ())
