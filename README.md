# portmidi-lisp
CFFI lisp interface to portmidi 2.0 
adapted and actualised from https://github.com/PortMidi/pm_cl

need installation of libportmidi for OSX or Linux before running

and also install quicklisp https://www.quicklisp.org/beta/  this is the simplest way for installing CFFI

I have just tested on sbcl but this should work with all Lisp implementation with CFFI port.

you have to look at the official portmidi doc

Work in progress ... adding writesysex

(load "cffi-portmidi3.lisp")

(in-package :portmidi)

(getDeviceInfo)
 ...
((:ID 0 :NAME "Midi Through Port-0" :TYPE :OUTPUT :OPEN T)
 (:ID 1 :NAME "Midi Through Port-0" :TYPE :INPUT :OPEN NIL))

PM> (defparameter midi-out (openoutput 0 100 0))

(defparameter syx  (string-to-sysex  "F0 00 21 F7"))

(writeSysex midi-out 0 syx)

(writesysex midi-out 0 (list-to-sysex '(#xF0 #x12 #xF7)))
