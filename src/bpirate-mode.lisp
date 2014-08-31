;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:bpirate-cl)

(defparameter +BB-MODE-SIG+         "BBIO1")
(defparameter +BB-SPI-MODE-SIG+     "SPI1")
(defparameter +BB-I2C-MODE-SIG+     "I2C1")
(defparameter +BB-UART-MODE-SIG+    "ART1")
(defparameter +BB-1WIRE-MODE-SIG+   "1W01")
(defparameter +BB-RAWWIRE-MODE-SIG+ "RAW1")

(defparameter +BB-RESET-CMD+           #b0)
(defparameter +BB-SPI-CMD+             #b1)
(defparameter +BB-I2C-CMD+             #b10)
(defparameter +BB-UART-CMD+            #b11)
(defparameter +BB-1WIRE-CMD+           #b100)
(defparameter +BB-RAWWIRE-CMD+         #b101)
(defparameter +BB-JTAG-CMD+            #b110)
(defparameter +BB-VP-MEASURE-CMD+      #b10100)
(defparameter +BB-VP-MEASURE-FLOW-CMD+ #b10101)
(defparameter +BB-FREQ-MEASURE-CMD+    #b10110)

;; PWM
(defparameter +BP-OSC-FREQ+ 32000000)
(defparameter +BP-PRESCALER-VALUES+ '(1 0 8 1 64 2 256 3)) ;; Plist
(defparameter +BB-PWM-CMD+     #b10010)
(defparameter +BB-PWM-CLR-CMD+ #b10011)

;; UART
(defparameter +BP-UART-PARITY-8/N+ 0)
(defparameter +BP-UART-PARITY-8/E+ 1)
(defparameter +BP-UART-PARITY-8/O+ 2)
(defparameter +BP-UART-PARITY-9/N+ 3)

(defparameter +BP-UART-STOPBIT/1+ 0)
(defparameter +BP-UART-STOPBIT/2+ 1)

(defparameter +BP-UART-PINOUT/HiZ  0)
(defparameter +BP-UART-PINOUT/3.3V 1)

(defparameter +BP-UART-POLARITY/1+ 0)
(defparameter +BP-UART-POLARITY/0+ 1)

(defclass bpirate-mode ()
  ((signature :initarg :signature)))

(defgeneric bpirate-mode-start (obj s &key &allow-other-keys))

(defgeneric bpirate-mode-stop (obj s &key &allow-other-keys))

(defgeneric bpirate-mode-reset (obj s &key &allow-other-keys))

(defclass bpirate-uart-mode (bpirate-mode)
  ((bridge :accessor uart-bridge
	   :initform nil)
   (baud :accessor uart-baud
	 :initform sb-posix:B115200)
   (stopbit :accessor uart-stopbit
	    :initform +BP-UART-STOPBIT/1+)
   (polarity :accessor uart-polarity
	     :initform +BP-UART-POLARITY/1+)
   (parity :accessor uart-parity
	   :initform +BP-UART-PARITY-8/N+)
   (pinout :accessor uart-pinout
	   :initform +BP-UART-PINOUT/HiZ)))

(defclass bpirate-spi-mode (bpirate-mode)
  ())

(defclass bpirate-pwm-mode (bpirate-mode)
  ((ocr :accessor pwm-ocr)
   (pry :accessor pwm-pry)
   (tcy :accessor pwm-tcy)))

(defmethod bpirate-mode-start ((obj bpirate-pwm-mode) s
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
      (with-bp-cmd (out s cmd)
	out))))

(defmethod bpirate-mode-stop ((obj bpirate-pwm-mode) s &key &allow-other-keys)
  (with-bp-cmd (out s (make-array 1 :element-type '(unsigned-byte 8)
				  :initial-element +BB-PWM-CLR-CMD+))
    out))
