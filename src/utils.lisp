;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:bpirate-cl)

(defun oct-to-string (oct)
  (let* ((len (length oct))
	(str (make-string len)))
    (loop for x from 0 below len do
	 (setf (char str x) (code-char (aref oct x)))
	 finally (return str))))
