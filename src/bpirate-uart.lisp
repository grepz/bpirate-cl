;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:bpirate-cl)

(defparameter +BP-UART-SPEED+
  (list sb-posix:B300    #b0000
	sb-posix:B1200   #b0001
	sb-posix:B2400   #b0010
	sb-posix:B4800   #b0011
	sb-posix:B9600   #b0100
	sb-posix:B19200  #b0101
	;; 0110=31250
	sb-posix:B38400  #b0111
	sb-posix:B57600  #b1000
	sb-posix:B115200 #b1001))

(defparameter +BP-UART-PARITY-8/N+ 0)
(defparameter +BP-UART-PARITY-8/E+ 1)
(defparameter +BP-UART-PARITY-8/O+ 2)
(defparameter +BP-UART-PARITY-9/N+ 3)

(defparameter +BP-UART-STOPBIT/1+ 0)
(defparameter +BP-UART-STOPBIT/2+ 1)

(defparameter +BP-UART-PINOUT/HiZ+  0)
(defparameter +BP-UART-PINOUT/3.3V+ 1)

(defparameter +BP-UART-POLARITY/1+ 0)
(defparameter +BP-UART-POLARITY/0+ 1)

(defparameter +BP-UART-VERSION-CMD+ #b1
  "00000001 – Display mode version string, responds 'ARTx'

Once in binary UART mode, send 0×01 to get the current mode version
string. The Bus Pirate responds ‘ARTx’, where x is the binary UART protocol
version (currently 1). Get the version string at any time by sending 0×01
again. This command is the same in all binary modes, the current mode can
always be determined by sending 0x01.")

(defparameter +BP-UART-ECHO-CMD-MASK+ #b10
  "0000001x – Start (0)/stop(1) echo UART RX

In binary UART mode the UART is always active and receiving. Incoming data
is only copied to the USB side if UART RX echo is enabled. This allows you
to configure and control the UART mode settings without random data
colliding with response codes. UART mode starts with echo disabled. This
mode has no impact on data transmissions.

Responds 0x01. Clears buffer overrun bit. ")

(defparameter +BP-UART-BAUDCFG-CMD+ #b111
  "00000111 – Manual baud rate configuration, send 2 bytes

Configures the UART using custom baud rate generator settings. This command
is followed by two data bytes that represent the BRG register value. Send the
high 8 bits first, then the low 8 bits.

Use the UART manual or an online calculator to find the correct value
(key values: fosc 32mHz, clock divider = 2, BRGH=1) . Bus Pirate responds 0x01
to each byte. Settings take effect immediately.")

(defparameter +BP-UART-BRIDGE-CMD+ #b1111
  "00001111 - UART bridge mode (reset to exit)

Starts a transparent UART bridge using the current configuration. Unplug
the Bus Pirate to exit.")

(defparameter +BP-UART-WRITE-CMD-MASK+ #b10000
  "0001xxxx – Bulk UART write, send 1-16 bytes (0=1byte!)

Bulk write transfers a packet of xxxx+1 bytes to the UART. Up to 16 data
bytes can be sent at once. Note that 0000 indicates 1 byte because there’s no
reason to send 0. BP replies 0×01 to each byte.")

(defparameter +BP-UART-PERIPHCFG-CMD-MASK+ #b1000000
  "0100wxyz – Configure peripherals w=power, x=pullups, y=AUX, z=CS

Enable (1) and disable (0) Bus Pirate peripherals and pins. Bit w enables the
power supplies, bit x toggles the on-board pull-up resistors, y sets the state
of the auxiliary pin, and z sets the chip select pin. Features not present in
a specific hardware version are ignored. Bus Pirate responds 0×01 on success.

Note: CS pin always follows the current HiZ pin configuration. AUX is always a
normal pin output (0=GND, 1=3.3volts).")

(defparameter +BP-UART-BAUDSET-CMD-MASK+ #b1100000
  "0110xxxx - Set UART speed

Set the UART at a preconfigured speed value: 0000=300, 0001=1200,
0010=2400,0011=4800,0100=9600,0101=19200,0110=31250 (MIDI),
0111=38400,1000=57600,1010=115200

Start default is 300 baud. Bus Pirate responds 0×01 on success. A read command
is planned but not implemented in this version.")

(defparameter +BP-UART-CONF-CMD-MASK+ #b10000000
  "100wxxyz – Configure UART settings

    w= pin output HiZ(0)/3.3v(1)
    xx=databits and parity 8/N(0), 8/E(1), 8/O(2), 9/N(3)
    y=stop bits 1(0)/2(1)
    z=RX polarity idle 1 (0), idle 0 (1)

Startup default is 00000. Bus Pirate responds 0x01 on success. A read command
is planned but not implemented in this version.

Note: that this command code is three bits because the databits and parity
setting consists of two bits. It is not quite the same as the binary SPI mode
configuration command code.")

(defclass bpirate-uart-mode (bpirate-mode)
  ((bridge :accessor uart-bridge)
   (baud :accessor uart-baud)
   (stopbit :accessor uart-stopbit)
   (polarity :accessor uart-polarity)
   (parity :accessor uart-parity)
   (pinout :accessor uart-pinout)))

(defmethod bpirate-mode-start ((obj bpirate-uart-mode) stream
			       &key (arg-baud sb-posix:B115200)
				 (arg-stopbit +BP-UART-STOPBIT/1+)
				 (arg-polarity +BP-UART-POLARITY/1+)
				 (arg-parity +BP-UART-PARITY-8/N+)
				 (arg-pinout +BP-UART-PINOUT/HiZ+) arg-bridge
				 &allow-other-keys)
  (with-slots (bridge baud stopbit polarity parity pinout) obj
      (setf bridge arg-bridge
	    baud arg-baud
	    stopbit arg-stopbit
	    polarity arg-polarity
	    parity arg-parity
	    pinout arg-pinout)
      (with-bp-cmd (out stream (make-array 1 :initial-element +BB-UART-CMD+
					   :element-type '(unsigned-byte 8))
			:timeout 1)
	(format t "Starting UART mode, output '~a'" out)
	(flexi-streams:octets-to-string out))))

(defmethod bpirate-mode-stop ((obj bpirate-uart-mode) stream
			      &key &allow-other-keys)
  (with-bp-cmd (out stream (make-array 1 :initial-element +BB-RESET-CMD+
				       :element-type '(unsigned-byte 8))
		    :timeout 1)
    (flexi-streams:octets-to-string out)))

(defmethod bpirate-uart-write ((obj bpirate-uart-mode) stream data)
    (process-data-chunk stream data 16 'bpirate-write-chunk))

(defun bpirate-write-chunk (stream chunk sz)
  (let ((cmd (logior +BP-UART-WRITE-CMD-MASK+ (1- sz))))
    (format t "Writing chunk(#~b) ~(~a, ~)/~a~%" cmd chunk sz)
    (with-bp-cmd (out stream (make-array 1 :element-type '(unsigned-byte 8)
					 :initial-element cmd))
      (when (/= (aref out 0) 1)
	(return-from bpirate-write-chunk 0)))
    (with-bp-cmd (out stream chunk :timeout 0.5)
      (loop for x across out with cnt = 0
	 when (= x 1) do (incf cnt)
	 finally (return cnt)))))

(defmethod bpirate-uart-echo ((obj bpirate-uart-mode) stream on)
  (let ((cmd (logior +BP-UART-ECHO-CMD-MASK+ on)))
    (format t "Setting UART echo ~[start~;stop~]~%" on)
    (with-bp-cmd (out stream (make-array 1 :element-type '(unsigned-byte 8)
					 :initial-element cmd))
      out)))

(defmethod bpirate-uart-speed ((obj bpirate-uart-mode) stream speed)
  (let ((cmd (logior +BP-UART-BAUDSET-CMD-MASK+
		     (getf +BP-UART-SPEED+ speed))))
    (format t "Setting UART speed: #~b~%" cmd)
    (with-bp-cmd (out stream (make-array 1 :element-type '(unsigned-byte 8)
					 :initial-element cmd))
      out)))

(defmethod bpirate-uart-config ((obj bpirate-uart-mode) stream
				&key polarity stopbit parity pinout)
  (let ((cmd (logior +BP-UART-CONF-CMD-MASK+
		     (if polarity polarity    (slot-value obj 'polarity))
		     (ash (if stopbit stopbit (slot-value obj 'stopbit)) 1)
		     (ash (if parity parity   (slot-value obj 'parity)) 2)
		     (ash (if pinout pinout   (slot-value obj 'pinout)) 4))))
    (format t "Setting UART config: #~b~%" cmd)
    (with-bp-cmd (out stream (make-array 1 :element-type '(unsigned-byte 8)
					 :initial-element cmd))
	out)))

(defmethod bpirate-uart-periph ((obj bpirate-uart-mode) stream
				&key (power 0) (pullups 0) (aux 0) (cs 0))
  (let ((cmd (logior +BP-UART-PERIPHCFG-CMD-MASK+
		     cs (ash aux 1) (ash pullups 2) (ash power 3))))
    (format t "Setting UART periph: #~b~%" cmd)
    (with-bp-cmd (out stream (make-array 1 :element-type '(unsigned-byte 8)
					 :initial-element cmd))
      out)))
