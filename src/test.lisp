;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:bpirate-cl)

(defparameter *test* nil)

;; XXX: Start

(setq *test* nil)
(setq *test* (make-instance 'bpirate :path "/dev/bp4" :bbmode t))
(bpirate-status *test*)

;; XXX: PWM
(bpirate-init-mode *test* :pwm)
(bpirate-mode-start (bpirate-mode *test*) (bpirate-stream *test*)
		    :prescaler 1 :period 0.001 :cycle 0.5)
(bpirate-mode-stop (bpirate-mode *test*) (bpirate-stream *test*))
(bpirate-deinit-mode *test*)
;; XXX: Self-test
(bpirate-init-mode *test* :test)
(bpirate-mode-start (bpirate-mode *test*) (bpirate-stream *test*))
(bpirate-mode-stop (bpirate-mode *test*) (bpirate-stream *test*))
(bpirate-deinit-mode *test*)
;; XXX: UART
(bpirate-init-mode *test* :uart)
(bpirate-mode-start (bpirate-mode *test*) (bpirate-stream *test*))
(bpirate-mode-stop (bpirate-mode *test*) (bpirate-stream *test*))
(bpirate-deinit-mode *test*)

#+nil(bpirate-uart-write (bpirate-mode *test*) (bpirate-stream *test*)
			 (make-array 5
			     :element-type '(unsigned-byte 8)
			     :initial-contents
			     (mapcar 'char-int '(#\t #\e #\s #\t
						 #\linefeed))))

(bpirate-uart-echo (bpirate-mode *test*) (bpirate-stream *test*) 1)
(bpirate-uart-speed (bpirate-mode *test*) (bpirate-stream *test*)
		    sb-posix:B115200)
(bpirate-uart-config (bpirate-mode *test*) (bpirate-stream *test*)
		     :pinout +BP-UART-PINOUT/HiZ+)
(bpirate-uart-periph (bpirate-mode *test*) (bpirate-stream *test*)
		     :power 1 :pullups 0 :aux 1 :cs 0)

#+nil(loop for x from 0 to 1000
	   with data = (make-array 19
				   :element-type '(unsigned-byte 8)
				   :initial-contents
				   (mapcar 'char-int '(#\t #\e #\s #\t #\! #\! #\! #\!
						       #\@ #\# #\$ #\% #\^ #\&
						       #\f #\o #\o
						       #\linefeed #\newline)))
	   do
	   (bpirate-uart-write (bpirate-mode *test*) (bpirate-stream *test*)
			       data)
	   (sleep 0.5))

#+nil(loop for x from 0 to 1000
	for data = (serial-read (bpirate-stream *test*)) do
	  (sleep 0.1)
	  (when (/= (length data) 0)
	    (print data)))

#+nil(loop for x across (serial-read (bpirate-stream *test*))
	   collect
	   (code-char x))

;; XXX: Stop
(bpirate-bbmode *test* :mode-on nil)
(bpirate-stop *test*)


;;;
(bpirate-bbmode *test* :mode-on t)
(bpirate-status *test*)
