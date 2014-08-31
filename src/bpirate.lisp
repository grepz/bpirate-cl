;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:bpirate-cl)

(defparameter +BP-OSC-FREQ+ 32000000)
(defparameter +BP-PRESCALER-VALUES+ '(1 0 8 1 64 2 256 3)) ;; Plist

(defparameter +BB-TRY+ 20)

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
(defparameter +BB-BP-RESET-CMD+        #b1111)
(defparameter +BB-SELFTEST-SHORT-CMD+  #b10000)
(defparameter +BB-SELFTEST-LONG-CMD+   #b10001)
(defparameter +BB-PWM-CMD+             #b10010)
(defparameter +BB-PWM-CLR-CMD+         #b10011)
(defparameter +BB-VP-MEASURE-CMD+      #b10100)
(defparameter +BB-VP-MEASURE-FLOW-CMD+ #b10101)
(defparameter +BB-FREQ-MEASURE-CMD+    #b10110)

(defparameter +BB-PINOUT-MASK+ #b01000000)
(defparameter +BB-ONOFF-MASK+  #b10000000)

(defparameter +BB-MODE-EXIT+ #xFF)

(defmacro with-bp-cmd ((out s cmd &key (timeout 0.1)) &body body)
  `(let ((,out (gensym "CMD-OUT")))
     (progn
       (serial-write ,s ,cmd)
       (sleep ,timeout)
       (setf ,out (serial-read ,s))
       ,@body)))

(defclass bpirate ()
  ((stream :reader bpirate-stream)
   (mode :reader bpirate-mode)
   (baud :accessor bpirate-baud)
   (device :accessor bpirate-device)
   (status :reader bpirate-status
	   :initform '(:stream nil :bbmode nil))))

(defmethod initialize-instance :after ((obj bpirate)
				  &key path (baud-key sb-posix:B115200) bbmode)
  (with-slots (stream status baud device) obj
    (setf baud   baud-key
	  device path)
    (bpirate-start obj)
    (when bbmode
      (bpirate-bbmode obj))))

(defgeneric bpirate-start (obj))

(defmethod bpirate-start ((obj bpirate))
  (with-slots (stream status baud device) obj
    (unless (getf status :stream)
      (setf stream (serial-open device baud))
      (when stream
	(setf (getf status :stream) t)
	(serial-read stream)))))

(defgeneric bpirate-stop (obj))

(defmethod bpirate-stop ((obj bpirate))
  (with-slots (stream status) obj
    (when (getf status :stream)
      ;; Close device stream
      (close stream)
      (setf (getf status :stream) nil))))

(defgeneric bpirate-bbmode (obj &key mode-on))

(defmethod bpirate-bbmode ((obj bpirate) &key (mode-on t))
  (with-slots (stream status) obj
    (when (getf (slot-value obj 'status) :stream)
      (if mode-on
	  ;; Turn ON
	  (unless (getf status :bbmode)
	    (with-bp-cmd (out stream (make-sequence 'vector +BB-TRY+))
	      (when (string= +BB-MODE-SIG+ (flexi-streams:octets-to-string out))
		(setf (getf status :bbmode) t))))
	  ;; Turn OFF
	  (when (getf status :bbmode)
	    (with-bp-cmd (out stream
			      (make-array 1 :initial-element +BB-BP-RESET-CMD+
					  :element-type '(unsigned-byte 8)))
	      (format t "~a" (flexi-streams:octets-to-string out))
	      (setf (getf status :bbmode) nil)))))))

(defmethod self-test ((obj bpirate) &key long-test)
  (let ((cmd (if long-test
		 +BB-SELFTEST-LONG-CMD+
		 +BB-SELFTEST-SHORT-CMD+))
	result)
    (with-slots (stream status) obj
      (when (and (getf status :stream) (getf status :bbmode))
	(with-bp-cmd (out stream (make-array 1 :initial-element cmd
					     :element-type '(unsigned-byte 8))
			  :timeout 3)
	  (setf result out))
	(with-bp-cmd (out stream (make-array
				  1 :initial-element +BB-MODE-EXIT+
				  :element-type '(unsigned-byte 8))))))
    (when result
      (aref result 0))))

(defmethod pwm-on ((obj bpirate) prescaler period cycle)
  (let* ((Tcy (/ 2.0 +BP-OSC-FREQ+))
	 (PRy (floor (1- (/ period (* Tcy prescaler)))))
	 (OCR (floor (* PRy cycle)))
	 (cmd (make-array 6
		:element-type '(unsigned-byte 8)
		:initial-contents
		(list +BB-PWM-CMD+ (getf +BP-PRESCALER-VALUES+ prescaler)
		      (logand (ash OCR -8) #xFF) (logand OCR #xFF)
		      (logand (ash PRy -8) #xFF) (logand PRy #xFF)))))
    (format t "Setup. Period: ~a, Tcy: ~a, Prescaler: ~a, PRy: ~a, OCR: ~a.~%"
	    period Tcy prescaler PRy OCR)
    (with-bp-cmd (out (slot-value obj 'stream) cmd)
      out)))

(defmethod pwm-off ((obj bpirate))
    (with-bp-cmd (out (slot-value obj 'stream)
		      (make-array 1 :element-type '(unsigned-byte 8)
				  :initial-element +BB-PWM-CLR-CMD+))
      out))

(defgeneric bpirate-init-mode (obj mode))

(defmethod bpirate-init-mode ((obj bpirate) bpmode)
  (with-slots (mode) obj
    (setf mode
       (cond ((eq bpmode :uart)
	      (make-instance 'bpirate-uart-mode))
	     ((eq bpmode :spi)
	      (make-instance 'bpirate-spi-mode))
	     (t nil)))))

;;(bpirate-init-mode *test* :uart)

;;(pwm-on *test* 1 0.001 0.5)
;;(pwm-off *test*)

;;(self-test *test*)

;;(serial-write (slot-value test 'stream) (make-sequence 'vector +BB-TRY+))
;;(serial-read (slot-value test 'stream))
;;(serial-recv-len (slot-value test 'stream))

(defparameter *test* nil)

;;(setq test nil)
;;(setq *test* (make-instance 'bpirate :path "/dev/bp4"))
;;(bpirate-bbmode *test*)
;;(bpirate-bbmode *test* :mode-on nil)
;;(bpirate-status *test*)
;;(bpirate-stop *test*)
;;(bpirate-start *test*)
;;(close (bpirate-stream *test*))
