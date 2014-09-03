;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:bpirate-cl)

(defparameter +BP-OSC-FREQ+ 32000000)
(defparameter +BP-PRESCALER-VALUES+ '(1 0 8 1 64 2 256 3)) ;; Plist
(defparameter +BB-PWM-CMD+     #b10010)
(defparameter +BB-PWM-CLR-CMD+ #b10011)

(defclass bpirate-pwm-mode (bpirate-mode)
  ((ocr :accessor pwm-ocr)
   (pry :accessor pwm-pry)
   (tcy :accessor pwm-tcy)))

(defmethod bpirate-mode-start ((obj bpirate-pwm-mode) stream
			       &key prescaler period cycle &allow-other-keys)
  (with-slots (ocr pry tcy) obj
    (setf tcy (/ 2.0 +BP-OSC-FREQ+)
	  pry (floor (1- (/ period (* Tcy prescaler))))
	  ocr (floor (* PRy cycle)))
    (let ((cmd (make-array 6
		 :element-type '(unsigned-byte 8)
		 :initial-contents
		 (list +BB-PWM-CMD+ (getf +BP-PRESCALER-VALUES+ prescaler)
		       (logand (ash OCR -8) #xFF) (logand OCR #xFF)
		       (logand (ash PRy -8) #xFF) (logand PRy #xFF)))))
      (format t "Setup. Period: ~a, Tcy: ~a, Prescaler: ~a, PRy: ~a, OCR: ~a.~%"
	      period Tcy prescaler PRy OCR)
      (with-bp-cmd (out stream cmd)
	out))))

(defmethod bpirate-mode-stop ((obj bpirate-pwm-mode) stream
			      &key &allow-other-keys)
  (with-bp-cmd (out stream (make-array 1 :element-type '(unsigned-byte 8)
				       :initial-element +BB-PWM-CLR-CMD+))
    out))
