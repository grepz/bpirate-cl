;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:bpirate-cl)

(defmacro with-bp-cmd ((var s cmd &key (timeout 0.1)) &body body)
  `(let ((,var (gensym)))
     (serial-write ,s ,cmd)
     (sleep ,timeout)
     (setf ,var (serial-read ,s))
     ,@body))

(defun oct-to-string (oct)
  (let* ((len (length oct))
	(str (make-string len)))
    (loop for x from 0 below len do
	 (setf (char str x) (code-char (aref oct x)))
	 finally (return str))))

(defun process-data-chunk (stream data chunk fn)
  (loop
     with data-len = (length data)
     with ret      = 0
     with cpos     = 0
     for diff-len  = (- data-len cpos)
     for chunk-len = (if (>= diff-len chunk) chunk diff-len) do
       (setf ret
	     (apply fn (list stream (subseq data cpos (+ cpos chunk-len))
			     chunk-len)))
       (setf cpos (+ cpos chunk-len))
     summing ret into total
     while
       (and (< cpos data-len) (= ret chunk-len))
     finally
       (return total)))

(defmacro test-list (lst f)
  (declare (symbol f)
	   (list lst))
  `(loop for l in ,lst always `(,@(funcall ,f l))))
