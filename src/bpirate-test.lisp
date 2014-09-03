;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:bpirate-cl)

(defparameter +BB-SELFTEST-SHORT-CMD+ #b10000)
(defparameter +BB-SELFTEST-LONG-CMD+  #b10001)

(defclass bpirate-test-mode (bpirate-mode)
  ())

(defmethod bpirate-mode-start ((obj bpirate-test-mode) stream
			       &key long-test &allow-other-keys)
  (with-bp-cmd (out stream (make-array 1
			      :initial-element (if long-test
						   +BB-SELFTEST-LONG-CMD+
						   +BB-SELFTEST-SHORT-CMD+)
			      :element-type '(unsigned-byte 8))
		    :timeout 1)
    out))

(defmethod bpirate-mode-stop ((obj bpirate-test-mode) stream
			      &key &allow-other-keys)
  (with-bp-cmd (out stream (make-array 1 :initial-element +BB-MODE-EXIT+
				       :element-type '(unsigned-byte 8))
		    :timeout 1)
    out))
