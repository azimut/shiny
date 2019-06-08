(in-package :shiny)

;; Helpers for sending UDP messages to a csound instance initilize
;; with kunstmusik/csound-live-code.
;;
;; $ csound --port=10000 livecode.csd
;;
;; Reference:
;; https://csound.com/docs/manual/udpserver.html
;; http://write.flossmanuals.net/csound/web-based-csound/
;; https://github.com/LispCookbook/cl-cookbook/blob/95086d1f8f5d64b3c4ec83523fbaba0e1ac52447/sockets.md

(ql:quickload :usocket)

(defvar *csound-socket* nil)
(defvar *csound-host* "127.0.0.1")
(defvar *csound-port* 10000)

(defun csound-socket-connect ()
  "initialize global usocket *CSOUND-SOCKET*"
  (unless *csound-socket*
    (setf *csound-socket*
          (usocket:socket-connect *csound-host* *csound-port*
                                  :protocol :datagram)))
  t)

(defun csound-socket-close ()
  "closes and nils global usocket *CSOUND-SOCKET*"
  (when *csound-socket*
    (usocket:socket-close *csound-socket*)
    (setf *csound-socket* nil))
  t)

(defun csound-socket-send (msg)
  "sends MSG through global usocket *CSOUND-SOCKET*"
  (declare (type string msg))
  (usocket:socket-send *csound-socket* msg (length msg)))

(defun csound-set-tempo (tempo)
  "sends \"set_tempo\" opcode"
  (declare (type fixnum tempo))
  (csound-socket-send (format nil "set_tempo ~d" tempo)))

(defun csound-chnset (i s)
  "sends \"chnset\" opcode"
  (declare (type number i) (type string s))
  (let ((msg (format nil "chnset ~a,\"~a\"" i s)))
    (csound-socket-send msg)))

;; FIXME: redefinition of csound.lisp function, what (make-play) uses.
#+nil
(defun csound-send-event (iname duration vars)
  "iname,0,duration,freq,amp
   Example:
   schedule \"Sub2\",0,.4,330,.3"
  (declare (type string iname) (type number duration) (type list vars))
  (let ((msg (format nil "schedule ~a,0,~a,~{~A~^,~}"
                     iname duration vars)))
    (csound-socket-send msg)))

(defun csound-schedule-msg (i duration keynum velocity)
  "format schedule event"
  (format nil "schedule ~a,0,~a,~a,~a"
          (alexandria:clamp i 8 44)
          duration (midihz keynum)
          (map-range 0 127 0f0 1f0 velocity)))
(defun csound-schedule-msg-s (i duration keynum velocity)
  "format schedule event when instrument I is a string"
  (format nil "schedule \"~a\",0,~a,~a,~a"
          i
          duration (midihz keynum)
          (map-range 0 127 0f0 1f0 velocity)))

;;--------------------------------------------------
;; NOTE: duration is divided by 4 since that matches
(defgeneric clc (i keynum velocity duration))

(defmethod clc ((i fixnum) (keynum fixnum) (velocity fixnum) duration)
  "single-note, instrument number"
  (let ((msg (csound-schedule-msg i (/ duration 4) keynum velocity)))
    (when (and *csound-socket* (> keynum 0) (> velocity 0) (> duration 0))
      (csound-socket-send msg))
    keynum))
(defmethod clc ((i string) (keynum fixnum) (velocity fixnum) duration)
  "single note, instrument string"
  (let ((msg (csound-schedule-msg-s i (/ duration 4) keynum velocity)))
    (when (and *csound-socket* (> keynum 0) (> velocity 0) (> duration 0))
      (csound-socket-send msg))
    keynum))
;; NOTE: avoid using clc again inside to have more control on aux methods
(defmethod clc ((i fixnum) (keynum list) (velocity fixnum) duration)
  "chord, instrument number"
  (mapc (lambda (k) (let ((msg (csound-schedule-msg i (/ duration 4) k velocity)))
                 (when (and *csound-socket* (> k 0) (> velocity 0) (> duration 0))
                   (csound-socket-send msg))))
        keynum))
(defmethod clc ((i string) (keynum list) (velocity fixnum) duration)
  "chord, instrument string"
  (mapc (lambda (k)
          (let ((msg (csound-schedule-msg-s i (/ duration 4) k velocity)))
            (when (and *csound-socket* (> k 0) (> velocity 0) (> duration 0))
              (csound-socket-send msg))))
        keynum))
