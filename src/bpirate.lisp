;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:bpirate-cl)

(defparameter +BB-TRY+ 20)

(defparameter +BB-BP-RESET-CMD+ #b1111)

(defparameter +BB-PINOUT-MASK+ #b01000000)
(defparameter +BB-ONOFF-MASK+  #b10000000)

(defparameter +BB-MODE-EXIT+ #xFF)

(defclass bpirate ()
  ((stream :reader bpirate-stream)
   (mode :reader bpirate-mode)
   (baud :accessor bpirate-baud)
   (device :accessor bpirate-device)
   (status :reader bpirate-status
	   :initform '(:stream nil :bbmode nil))))

(defmethod initialize-instance :after ((obj bpirate)
				  &key path (baud-key sb-posix:B115200) bbmode)
  (with-slots (baud device) obj
    (setf baud baud-key
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
	    (with-bp-cmd (out stream (make-sequence 'vector +BB-TRY+)
			      :timeout 1)
	      (when (string= +BB-MODE-SIG+ (flexi-streams:octets-to-string out))
		(setf (getf status :bbmode) t))))
	  ;; Turn OFF
	  (when (getf status :bbmode)
	    (with-bp-cmd (out stream
			      (make-array 1 :initial-element +BB-BP-RESET-CMD+
					  :element-type '(unsigned-byte 8)))
	      (format t "~a" (flexi-streams:octets-to-string out))
	      (setf (getf status :bbmode) nil)))))))

(defgeneric bpirate-init-mode (obj mode))

(defmethod bpirate-init-mode ((obj bpirate) bpmode)
  (with-slots (mode) obj
    (setf mode
	  (cond ((eq bpmode :pwm)
		 (make-instance 'bpirate-pwm-mode))
		((eq bpmode :uart)
		 (make-instance 'bpirate-uart-mode))
		((eq bpmode :spi)
		 (make-instance 'bpirate-spi-mode))
		((eq bpmode :test)
		 (make-instance 'bpirate-test-mode))
		(t nil)))))

(defgeneric bpirate-deinit-mode (obj))

(defmethod bpirate-deinit-mode ((obj bpirate))
  (setf (slot-value obj 'mode) nil))
