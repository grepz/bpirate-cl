;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:bpirate-cl)

(defconstant +FIONREAD+ #x541B)

(defun serial-open (path baud &key (vmin 0) (vtime 1))
  ;; sb-posix:O-NDELAY sb-posix:O-NONBLOCK
  (let* ((fd (sb-posix:open path (logior sb-posix:O-NOCTTY sb-posix:O-RDWR)))
	 (old (sb-posix:tcgetattr fd))
	 (new (sb-posix:tcgetattr fd)))
    (setf
     ;; LFLAG
     (sb-posix:termios-lflag new)
     (logandc2 (sb-posix:termios-lflag old)
     	      (logior sb-posix:icanon sb-posix:echo sb-posix:echoe
     		      sb-posix:echok sb-posix:echonl))
     ;; CFLAG: baud | CS8 | CREAD | CLOCAL
     (sb-posix:termios-cflag new)
     (logior sb-posix:CS8 sb-posix:CREAD sb-posix:CLOCAL baud)
     ;; VMIN
     (aref (sb-posix:termios-cc new) sb-posix:vmin) vmin
     ;; VTIME
     (aref (sb-posix:termios-cc new) sb-posix:vtime) vtime)
    (sb-posix:tcflush fd sb-posix:TCIFLUSH)
    (sb-posix:tcsetattr fd sb-posix:TCSAFLUSH new)
    (make-fd-stream fd :input t :output t :buffering :none
		    :element-type '(unsigned-byte 8))))

(defun serial-close (s)
  ;; TODO:
  (unless (null s)
    (close s)))

(defun serial-recv-len (s)
  (sb-alien:with-alien ((bytes sb-alien:int))
    (sb-posix:ioctl (fd-stream-fd s) +FIONREAD+ (sb-alien:addr bytes))
    bytes))

(defun serial-read (s &key)
  (let ((n (serial-recv-len s)))
    (unless (zerop n))
      (let ((res (make-sequence 'vector n)))
	(loop for i from 0 below n do
	     (setf (aref res i) (read-byte s))
	   finally (return res)))))

(defun serial-write (s data)
  (declare (vector data))
  (loop for x across data do
       (write-byte x s)
     finally (finish-output s)))
