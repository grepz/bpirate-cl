;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:bpirate-cl)

(defparameter *test* nil)

;; XXX: Start
(setq *test* (make-instance 'bpirate :path "/dev/bp4" :bbmode t))
(bpirate-status *test*)
;; XXX: PWM
(bpirate-init-mode *test* :pwm)
(bpirate-mode-start (bpirate-mode *test*) (bpirate-stream *test*)
		    :prescaler 1 :period 0.001 :cycle 0.5)
(bpirate-mode-stop (bpirate-mode *test*) (bpirate-stream *test*))
(bpirate-deinit-mode *test*)


;; XXX: Stop
(bpirate-bbmode *test* :mode-on nil)
(bpirate-stop *test*)
