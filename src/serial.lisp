;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:bpirate-cl)

(defconstant +FIONREAD+ #x541B)

(defun serial-open (path baud)
  (let* ((dev (sb-posix:open
	       path (logior sb-posix:O-NOCTTY sb-posix:O-NDELAY
			    sb-posix:O-RDWR sb-posix:O-NONBLOCK
			    )))
	 (tcattr (sb-posix:tcgetattr dev)))
    (setf (sb-posix:termios-cflag tcattr)
	  ;; baud | CS8 | CREAD | CLOCAL
	  (logior sb-posix:CS8 sb-posix:CREAD sb-posix:CLOCAL baud))
    (sb-posix:tcflush dev sb-posix:TCIFLUSH)
    (sb-posix:tcsetattr dev sb-posix:TCSAFLUSH tcattr)
    (sb-sys:make-fd-stream dev :input t :output t :buffering :none
			   :element-type '(unsigned-byte 8))))

(defun serial-close (s)
  ;; TODO:
  (unless (null s)
    (close s)))

(defun serial-recv-len (s)
  (sb-alien:with-alien ((bytes sb-alien:int))
    (sb-posix:ioctl (sb-sys:fd-stream-fd s) +FIONREAD+ (sb-alien:addr bytes))
    bytes))

(defun serial-read (s)
  (let ((n (serial-recv-len s)))
    (unless (zerop n)
      (let ((res (make-sequence 'vector n)))
	(loop for i from 0 below n do
	     (setf (aref res i) (read-byte s))
	   finally (return res))))))

(defun serial-write (s data)
  (declare (vector data))
  (loop for x across data do
       (write-byte x s)
       finally (finish-output s)))
