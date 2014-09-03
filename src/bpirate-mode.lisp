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

(defclass bpirate-mode ()
  ((signature :initarg :signature)))

(defgeneric bpirate-mode-start (obj s &key &allow-other-keys))

(defgeneric bpirate-mode-stop (obj s &key &allow-other-keys))

(defgeneric bpirate-mode-reset (obj s &key &allow-other-keys))
