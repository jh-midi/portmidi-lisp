;;; modified by jh-midi 2025
;;; tested with sbcl and ccl
;;; **********************************************************************
;;; Copyright (C) 2005 Heinrich Taube, <taube (at) uiuc (dot) edu>
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Lisp Lesser Gnu Public License.
;;; See http://www.cliki.net/LLGPL for the text of this agreement.
;;; **********************************************************************

;;; $Name: rel-2_0_0 $
;;; $Revision: .2 $
;;; $Date: 2025/01/08 15:00:28 $

;;; A CFFI interface to Portmidi. Should run in most Common Lisp
;;; implementations on Linux, OS X and Windows. For information about
;;; CFFI see http://common-lisp.net/project/cffi/

(ql:quickload :cffi) ;; JH

(in-package :cl-user)

(defvar *libportmidi*
  (let ((type #+(or darwin macos macosx) "dylib"
              #+(or linux linux-target (and unix pc386) freebsd) "so"
              #+(or win32 microsoft-32 cygwin) "dll")
        (paths (list "/usr/lib/" "/usr/lib64/"
		     "/usr/lib/x86_64-linux-gnu/"
		     "/usr/local/lib/" "/usr/local/lib64/"  *load-pathname*)))
    (loop for d in paths
       for p = (make-pathname :name "libportmidi" :type type 
                              :defaults d)
       when (probe-file p) do (return p)
       finally  
         (error "Library \"portmidi.~A\" not found. Fix *libportmidi*."
                type))))


;;; load portmidi lib

(cffi:load-foreign-library *libportmidi*)

(defpackage :portmidi
  (:use :common-lisp) 
  (:nicknames :pm :pt)
  (:shadow :initialize :terminate :time :start :stop :abort 
           :close :read :write :poll)
  (:export :Initialize :Terminate 
           :HasHostError :GetErrorText :GetHostErrorText 
           :CountDevices
           :GetDeviceInfo
           :Message :Message.status :Message.data1 :Message.data2
           :Event.message :Event.timestamp 
           ;; event buffers added to api
           :EventBufferNew :EventBufferFree :EventBufferElt
           :EventBufferMap
	   :CreateVirtualInput :CreateVirtualOutput :DeleteVirtualDevice
           :OpenInput :OpenOutput :SetFilter :SetChannelMask
           :Abort :Close :Read :Write :Poll :WriteShort :WriteSysex
           ;; filtering constants
           :filt-active :filt-sysex :filt-clock :filt-play :filt-f9 
           :filt-fd :filt-reset :filt-note :filt-channel-aftertouch
           :filt-poly-aftertouch :filt-program :filt-control
           :filt-pitchbend :filt-mtc :filt-song-position 
           :filt-song-select :filt-tune :filt-tick :filt-undefined
           :filt-realtime  :filt-aftertouch :filt-systemcommon
           ;; porttime.
           :Start :Stop :Started :Time
           ;; initialization insurers added to api
           :portmidi :*portmidi* ))

(in-package :portmidi)

(cffi:defcstruct pm-device-info 
		 (struct-version :int) 
		 (interf :string) 
		 (name :string) 
		 (input :int) 
		 (output :int) 
		 (opened :int))

;;(cffi:define-foreign-type pm-message () ':long)
;;(cffi:define-foreign-type pm-timestamp () ':long)
;; replaced by :
(cffi:defctype pm-message :long)
(cffi:defctype pm-timestamp :long)

(cffi:defcstruct pm-event 
		 (message pm-message) 
		 (timestamp pm-timestamp))
(cffi:defctype pm-error  :int)

(cffi:defctype port-midi-stream :void)
(cffi:defctype pm-device-id  :int)
(cffi:defctype pm-time-proc-ptr :pointer)


(cffi:defcfun ("Pm_WriteSysEx" pm-write-sys-ex) pm-error (stream :pointer) (when pm-timestamp) (msg :pointer)) 
(cffi:defcfun ("Pm_WriteShort" pm-write-short) pm-error (stream :pointer) (when pm-timestamp) (msg :long)) 
(cffi:defcfun ("Pm_Write" pm-write) pm-error (stream :pointer) (buffer :pointer) (length :long)) 
(cffi:defcfun ("Pm_Poll" pm-poll) pm-error (stream :pointer)) 
(cffi:defcfun ("Pm_Read" pm-read) pm-error (stream :pointer) (buffer :pointer) (length :long)) 
(cffi:defcfun ("Pm_Close" pm-close) pm-error (stream :pointer)) 
(cffi:defcfun ("Pm_Abort" pm-abort) pm-error (stream :pointer)) 
;(cffi:defcfun ("Pm_SetChannelMask" pm-set-channel-mask) pm-error (stream :pointer) (mask :int)) 
(cffi:defcfun ("Pm_SetFilter" pm-set-filter) pm-error (stream :pointer) (filters :long))


(cffi:defcfun ("Pm_OpenOutput" pm-open-output) pm-error (stream :pointer) (output-device pm-device-id) (output-driver-info :pointer) (buffer-size :long) (time-proc pm-time-proc-ptr) (time-info :pointer) (latency :long)) 
(cffi:defcfun ("Pm_OpenInput" pm-open-input) pm-error (stream :pointer) (input-device pm-device-id) (input-driver-info :pointer) (buffer-size :long) (time-proc pm-time-proc-ptr) (time-info :pointer))

(cffi:defcfun ("Pm_CreateVirtualInput" pm-create-virtual-input) pm-error (name :string)  (interf :string) (deviceinfo :pointer))
(cffi:defcfun ("Pm_CreateVirtualOutput" pm-create-virtual-output) pm-error (name :string)  (interf :string) (deviceinfo :pointer))
(cffi:defcfun ("Pm_DeleteVirtualDevice" pm-delete-virtual-device) pm-error (device-id :long))

(cffi:defcfun ("Pm_GetDeviceInfo" pm-get-device-info) :pointer (id pm-device-id)) 

(cffi:defcfun ("Pm_CountDevices" pm-count-devices) :int) 
(cffi:defcfun ("Pm_GetHostErrorText" pm-get-host-error-text) :void (msg :string) (len :unsigned-int)) 
(cffi:defcfun ("Pm_GetErrorText" pm-get-error-text) :string (errnum pm-error)) 
(cffi:defcfun ("Pm_HasHostError" pm-has-host-error) :int (stream :pointer)) 
(cffi:defcfun ("Pm_Terminate" pm-terminate) pm-error) 
(cffi:defcfun ("Pm_Initialize" pm-initialize) pm-error)

;;; porttime.h

(cffi:defctype pt-error :int)
(cffi:defctype pt-timestamp :long)
(cffi:defcfun ("Pt_Start" pt-start) pt-error (a :int) (b :pointer) (c :pointer))
(cffi:defcfun ("Pt_Stop" pt-stop) pt-error )
(cffi:defcfun ("Pt_Started" pt-started) :int)
(cffi:defcfun ("Pt_Time" pt-time) pt-timestamp)
 
(defconstant true 1) 
(defconstant false 0)
(defconstant pmNoError 0)
(defconstant pmHostError -10000)
(defconstant pm-no-device -1)
(defconstant pm-default-sysex-buffer-size 1024) 
(defconstant filt-active 1) 
(defconstant filt-sysex 2) 
(defconstant filt-clock 4) 
(defconstant filt-play 8) 
(defconstant filt-f9 16) 
(defconstant filt-fd 32) 
(defconstant filt-reset 64) 
(defconstant filt-note 128) 
(defconstant filt-channel-aftertouch 256) 
(defconstant filt-poly-aftertouch 512) 
(defconstant filt-program 1024) 
(defconstant filt-control 2048) 
(defconstant filt-pitchbend 4096) 
(defconstant filt-mtc 8192) 
(defconstant filt-song-position 16384) 
(defconstant filt-song-select 32768) 
(defconstant filt-tune 65536) 
(defconstant filt-tick filt-f9)
(defconstant filt-undefined (logior filt-f9 filt-fd))
(defconstant filt-realtime (logior filt-active filt-sysex
                                      filt-clock filt-play
                                      filt-undefined filt-reset))
(defconstant filt-aftertouch (logior filt-channel-aftertouch
                                        filt-poly-aftertouch ))
(defconstant filt-systemcommon (logior filt-mtc filt-song-position
                                          filt-song-select filt-tune))
(defvar *portmidi* nil) ; t if loaded

;;;
;;; utils
;;;

(defun Started ()
  (let ((res (pt-started)))
    (if (= res false) nil t)))

(defun Start ()
  ;; NB: This has to be called before opening output or input.
  ;; it seems that if its called 2x we get an error.
  (unless (Started)
    (pt-start 1 (cffi:null-pointer) (cffi:null-pointer))))


(defvar host-error-text (make-string 256 :initial-element #\*))

(defmacro with-pm-error (form)
  (let ((v (gensym)))
    `(let ((,v ,form))
       (if (not (= ,v pmNoError))
	   (if (= ,v pmHostError)
	       (cffi:with-foreign-string (host-error host-error-text)
		 (pm-get-host-error-text host-error
					 (length host-error-text))
		 (error "Host error is: ~a"
			(cffi:foreign-string-to-lisp host-error)))
	       (error (cffi:foreign-string-to-lisp
		       (pm-get-error-text ,v))))
           ,v))))

(defun portmidi ()
  ;; initializer, call before using lib
  (or *portmidi*
      (progn (pm-initialize)
             (setq *portmidi* t))))

(defun Message (status data1 data2)
  ;; portmidi messages are just unsigneds
  (logior (logand (ash data2 16) #xFF0000)
          (logand (ash data1 08) #xFF00)
          (logand status #xFF)))

(defun Message.status (m)
  (logand m #xFF))

(defun Message.data1 (m)
  (logand (ash m -08) #xFF))

(defun Message.data2 (m)
  (logand (ash m -16) #xFF))

;;; accessors 
;; original : JH comment
;;(defun DeviceInfo.interf (ptr)
;;  (cffi:foreign-string-to-lisp 
;;   (cffi:foreign-slot-value ptr 'pm-device-info 'interf)))

;; test new format
;; (setf ptr (pm::pm-get-device-info 4))
;; (cffi:foreign-slot-value ptr 'pm::pm-device-info 'interf) ;old call
;; (cffi:foreign-slot-names '(:struct pm::pm-device-info))
;; (cffi:foreign-slot-value ptr  '(:struct pm::pm-device-info) 'portmidi::interf) ; new call
;; JH deprecated call to struct replaced by :

(defun DeviceInfo.interf (ptr)
     (cffi:foreign-slot-value ptr '(:struct pm-device-info) 'interf))


(defun DeviceInfo.name (ptr)
    (cffi:foreign-slot-value ptr '(:struct pm-device-info) 'name))

(defun DeviceInfo.input (ptr)
  (if (= (cffi:foreign-slot-value ptr'(:struct pm-device-info) 'input) 0)
      nil
    t))

(defun DeviceInfo.output (ptr)
  (if (= (cffi:foreign-slot-value ptr'(:struct pm-device-info) 'output) 0)
      nil
    t))

(defun DeviceInfo.opened (ptr)
  (if (= (cffi:foreign-slot-value ptr '(:struct pm-device-info) 'opened) 0)
      nil
    t))

(defun Event.message (e &optional (v nil vp))
  (if vp
      (progn 
	(setf (cffi:foreign-slot-value e '(:struct pm-event) 'message) v)
	v)
    (cffi:foreign-slot-value e '(:struct pm-event) 'message)))
    
(defun Event.timestamp (e &optional (v nil vp))
  (if vp
      (progn 
	(setf (cffi:foreign-slot-value e '(:struct pm-event) 'timestamp) v)
	v)
    (cffi:foreign-slot-value e '(:struct pm-event) 'timestamp)))

;;; functions

(defun Initialize ()
  (with-pm-error (pm-initialize)))

(defun terminate ()
  (with-pm-error (pm-terminate)))


(defun HasHostError (pms) 
  (pm-has-host-error pms))

(defun GetHostErrorText ()
  (let ((host-error-text (make-string 256 :initial-element #\*)))
    (pm-get-host-error-text host-error-text 256)
  host-error-text))
    

(defun GetErrorText (err) 
    (pm-get-error-text err))


(defun CountDevices ()
  (portmidi)
  (pm-count-devices ))


(defun CreateVirtualInput (name &optional (interf "ALSA"))
  (unless (Started) (Start))
   (let ((err (pm-create-virtual-input name interf  (cffi:null-pointer))))
     (if (= err pmNoError)
	 err ;; alias for the new id
	 (error  (GetErrorText  err)))))

(defun CreateVirtualOutput (name &optional (interf "ALSA"))
  (unless (Started) (Start))
   (let ((err (pm-create-virtual-output name interf  (cffi:null-pointer))))
     (if (= err pmNoError)
	 err ;; alias for the new id
	 (error  (GetErrorText  err)))))

(defun DeleteVirtualDevice (device-id)
  (unless (Started) (Start))
   (let ((err (pm-delete-virtual-device device-id)))
     (if (= err pmNoError)
	 err ;; alias for the new id
	 (error  (GetErrorText  err)))))


(defun OpenInput (device bufsiz)
  ;; portmidi: timer must be running before opening
  (unless (Started) (Start))
  (cffi:with-foreign-object (p1 :pointer)
    (let ((err (pm-open-input p1 device (cffi:null-pointer)
                              bufsiz (cffi:null-pointer) (cffi:null-pointer))))
        (if (= err pmNoError)
            (cffi:mem-ref p1 :pointer)
            (error (pm-get-error-text err))))))

(defun OpenOutput (device bufsiz latency)
  (unless (Started) (Start))
  (cffi:with-foreign-object (p1 :pointer) ;(p (* PortMidi))
    (let ((err (pm-open-output p1 device (cffi:null-pointer)
                               bufsiz (cffi:null-pointer) (cffi:null-pointer)
                               latency)))
      (if (= err pmNoError)
          (cffi:mem-ref p1 :pointer)
          (error (pm-get-error-text err))))))

(defun SetFilter (a filts) 
  (with-pm-error
    (pm-set-filter a filts)))

;(defun SetChannelMask (pms mask)
;  (with-pm-error (pm-set-channel-mask pms mask)))

(defun Abort (pms)
  (with-pm-error (pm-abort pms)))

(defun Close (pms)
  (with-pm-error (pm-close pms)))

(defun EventBufferFree (buf)
  (cffi:foreign-free buf))

(defun EventBufferNew (len)
  (cffi:foreign-alloc '(:struct pm-event) :count len))

(defun EventBufferElt (buf i)
  ;; buf is POINTER to buf
 ;; (cffi:mem-aref buf 'pm-event i)) replaced by :
(cffi:mem-aptr buf '(:struct pm::pm-event) i ))

(defun EventBufferSet (buffer index timestamp message)
  (setf (cffi:foreign-slot-value
         (cffi:mem-aptr buffer '(:struct pm-event) index)
	 '(:struct pm-event) 'timestamp)
        timestamp)
  (setf (cffi:foreign-slot-value
         (cffi:mem-aptr buffer  '(:struct pm-event) index)
	 '(:struct pm-event) 'message)
        message)
  (values))

(defun EventBufferMap (fn buf end)
  (loop for i below end
     for e = (EventBufferElt buf i)
     do (funcall fn (Event.message e) (Event.timestamp e)))
 (values))

(defun Read (pms *evbuf len) 
  (let ((res (pm-read pms *evbuf len)))
    (if (< res 0)
        (error (pm-get-error-text res))
        res)))

(defun Poll (pms)
  (let ((res (pm-poll pms)))
    (cond ((= res 0) nil)
          ((= res 1) t)
          (t (error (pm-get-error-text res))))))

(defun Write (pms *evbuf len)
  (with-pm-error (pm-write pms *evbuf len)))

(defun WriteShort (pms when msg)
  (with-pm-error (pm-write-short pms when msg)))

#| (defparameter syx  (string-to-sysex  "F0 00 21
         50 00 01 00
 01 01 01 04 02 F7"))
|#
(defun string-to-sysex (hex-string)
  "allow multiple space and newline between hexa string "
  (let* ((taille (length hex-string))
	 (next1 0)
	 (to-hex (loop while (<= next1 (- taille 1))
		       collect
		       (multiple-value-bind (hex next)
			   (read-from-string hex-string t nil :start next1)
			 (setf next1 next)
			 (read-from-string (format nil "#x~d" hex ))))))
    (make-array (length to-hex)
		:element-type '(unsigned-byte 8)
		:initial-contents to-hex)))

(defun list-to-sysex (sysex-list)
  (make-array (length sysex-list)
		:element-type '(unsigned-byte 8)
		:initial-contents sysex-list))


(defun WriteSysex (pms when sysex-unsigned)
  (cffi:with-pointer-to-vector-data  (ptr sysex-unsigned )
   (pm-write-sys-ex pms when ptr)))

;;; porttime.h



(defun Stop ()
  (when (Started)
    (with-pm-error (pt-stop)))
  (values))

(defun Time ()
  (pt-time))

(defun GetDeviceInfo (&optional id)
   "portmidi return null pointer when id is deleted or out of range then I output nil for the id"
  (flet ((getone (id)
           (let ((d (pm-get-device-info id)))
	     (if (cffi:null-pointer-p d)
		 (list id nil)  
		 (list :id id
                   :name  (DeviceInfo.name d)
                   :type (if (DeviceInfo.input d) ':input ':output)
                   :open (DeviceInfo.opened d))))))
    ;; make sure lib is initialized before checking devices
    (portmidi)
    (if id (getone id)
        (loop for i below (CountDevices)
           collect (getone i)))))

(pushnew ':portmidi *features*)
