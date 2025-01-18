(defpackage :xclhb-xim
  (:use :cl)
  (:local-nicknames (:x :xclhb))
  (:export
   :set-filter-event-handler
   :create-xim
   :destroy-xim))

(in-package :xclhb-xim)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *debug-mode-p* nil))

(defmacro debug-print (fmt &rest args)
  (when *debug-mode-p*
    `(format *debug-io* ,fmt ,@args)))

(defconstant +xim-connect+ 1)
(defconstant +xim-connect-reply+ 2)
(defconstant +xim-disconnect+ 3)
(defconstant +xim-disconnect-reply+ 4)
(defconstant +xim-auth-required+ 10)
(defconstant +xim-auth-reply+ 11)
(defconstant +xim-auth-next+ 12)
(defconstant +xim-auth-setup+ 13)
(defconstant +xim-auth-ng+ 14)
(defconstant +xim-error+ 20)
(defconstant +xim-open+ 30)
(defconstant +xim-open-reply+ 31)
(defconstant +xim-close+ 32)
(defconstant +xim-close-reply+ 33)
(defconstant +xim-register-triggerkeys+ 34)
(defconstant +xim-trigger-notify+ 35)
(defconstant +xim-trigger-notify-reply+ 36)
(defconstant +xim-set-event-mask+ 37)
(defconstant +xim-encoding-negotiation+ 38)
(defconstant +xim-encoding-negotiation-reply+ 39)
(defconstant +xim-query-extension+ 40)
(defconstant +xim-query-extension-reply+ 41)
(defconstant +xim-set-im-values+ 42)
(defconstant +xim-set-im-values-reply+ 43)
(defconstant +xim-get-im-values+ 44)
(defconstant +xim-get-im-values-reply+ 45)
(defconstant +xim-create-ic+ 50)
(defconstant +xim-create-ic-reply+ 51)
(defconstant +xim-destroy-ic+ 52)
(defconstant +xim-destroy-ic-reply+ 53)
(defconstant +xim-set-ic-values+ 54)
(defconstant +xim-set-ic-values-reply+ 55)
(defconstant +xim-get-ic-values+ 56)
(defconstant +xim-get-ic-values-reply+ 57)
(defconstant +xim-set-ic-focus+ 58)
(defconstant +xim-unset-ic-focus+ 59)
(defconstant +xim-forward-event+ 60)
(defconstant +xim-sync+ 61)
(defconstant +xim-sync-reply+ 62)
(defconstant +xim-commit+ 63)
(defconstant +xim-reset-ic+ 64)
(defconstant +xim-reset-ic-reply+ 65)
(defconstant +xim-geometry+ 70)
(defconstant +xim-str-conversion+ 71)
(defconstant +xim-str-conversion-reply+ 72)
(defconstant +xim-preedit-start+ 73)
(defconstant +xim-preedit-start-reply+ 74)
(defconstant +xim-preedit-draw+ 75)
(defconstant +xim-preedit-caret+ 76)
(defconstant +xim-preedit-caret-reply+ 77)
(defconstant +xim-preedit-done+ 78)
(defconstant +xim-status-start+ 79)
(defconstant +xim-status-draw+ 80)
(defconstant +xim-status-done+ 81)
(defconstant +xim-preeditstate+ 82)

(defconstant +attr-type-separator-of-nested-list+ 0)
(defconstant +attr-type-byte-data+ 1)
(defconstant +attr-type-word-data+ 2)
(defconstant +attr-type-long-data+ 3)
(defconstant +attr-type-char-data+ 4)
(defconstant +attr-type-window+ 5)
(defconstant +attr-type-xim-styles+ 10)
(defconstant +attr-type-xrectangle+ 11)
(defconstant +attr-type-xpoint+ 12)
(defconstant +attr-type-xfont-set+ 13)
(defconstant +attr-type-xim-hot-key-triggers+ 15)
(defconstant +attr-type-xim-string-conversion+ 17)
(defconstant +attr-type-xim-preedit-state+ 18)
(defconstant +attr-type-xim-reset-state+ 19)
(defconstant +attr-type-nested-list+ #x7fff)

(defconstant +xim-string-conversion-left-edge+ #x1)
(defconstant +xim-string-conversion-right-edge+ #x2)
(defconstant +xim-string-conversion-top-edge+ #x4)
(defconstant +xim-string-conversion-bottom-edge+ #x8)
(defconstant +xim-string-conversion-convealed+ #x10)
(defconstant +xim-string-conversion-wrapped+ #x20)

(defconstant +xim-reverse+ #x1)
(defconstant +xim-underline+ #x2)
(defconstant +xim-highlight+ #x4)
(defconstant +xim-primary+ #x8)
(defconstant +xim-secondary+ #x10)
(defconstant +xim-tertiary+ #x20)
(defconstant +xim-visible-to-forward+ #x40)
(defconstant +xim-visible-to-backward+ #x80)
(defconstant +xim-visible-center+ #x100)

(defconstant +xim-hot-key-state-on+ #x1)
(defconstant +xim-hot-key-state-off+ #x2)

(defconstant +xim-preedit-enable+ #x1)
(defconstant +xim-preedit-disable+ #x2)

(defconstant +xim-initial-state+ #x1)
(defconstant +xim-preserve-state+ #x2)

(defconstant +xim-error-flag-both-id-invalid+ 0)
(defconstant +xim-error-flag-input-method-id-valid+ 1)
(defconstant +xim-error-flag-input-context-id-valid+ 2)

(defconstant +xim-error-bad-alloc+ 1)
(defconstant +xim-error-bad-style+ 2)
(defconstant +xim-error-bad-client-window+ 3)
(defconstant +xim-error-bad-focus-window+ 4)
(defconstant +xim-error-bad-area+ 5)
(defconstant +xim-error-bad-spot-location+ 6)
(defconstant +xim-error-bad-colormap+ 7)
(defconstant +xim-error-bad-atom+ 8)
(defconstant +xim-error-bad-pixel+ 9)
(defconstant +xim-error-bad-pixmap+ 10)
(defconstant +xim-error-bad-name+ 11)
(defconstant +xim-error-bad-cursor+ 12)
(defconstant +xim-error-bad-protocol+ 13)
(defconstant +xim-error-bad-foreground+ 14)
(defconstant +xim-error-bad-background+ 15)
(defconstant +xim-error-locale-not-supported+ 16)
(defconstant +xim-error-bad-something+ 999)

(defun intern-atom-sync (client atom-name)
  (let ((name (x:string->card8-vector atom-name)))
    (x:intern-atom-reply-atom (x:intern-atom-sync client 0 (length name) name))))

(x:defglobal *xim-reader-table* (make-array 256 :initial-element nil))

(defstruct xim
  client
  atom-xim-servers
  atom-xim-xconnect
  atom-xim-moredata
  atom-xim-protocol
  im-server-atom
  im-server-window
  client-window
  server-window
  (seq-no 0)
  (message-handler-table (make-array 256 :initial-element nil))
  (filtered-event-handler-table (make-array 256 :initial-element nil))
  on-commit
  input-method-id
  input-context-id
  im-attributes
  ic-attributes)

(defun send-client-message (client window format type data)
  "format must be 8 or 16 or 32"
  (let ((buf (x:make-buffer 32)))
    (x::write-card8 buf 0 33)
    (x::write-card8 buf 1 format)
    (x::write-card16 buf 2 0)
    (x::write-card32 buf 4 window)
    (x::write-card32 buf 8 type)
    (x::write-list-of-data buf 12 data (x::bytes-per-data format))
    (x:send-event client 0 window 0 buf)))

(defun send-message (xim data)
  (let ((client (xim-client xim))
        (server-window (xim-server-window xim))
        (len (length data)))
    (cond ((<= len 20)
           (debug-print "client -> server(client-message) ~a~%" data)
           (send-client-message client server-window 8 (xim-atom-xim-protocol xim) data)
           (x:flush client))
          (t
           (let* ((seq-no (mod (1+ (xim-seq-no xim)) 1000))
                  (name (x:string->card8-vector (format nil "_client~3,'0d" seq-no))))
             (setf (xim-seq-no xim) seq-no)
             (x:intern-atom client 
                            (lambda (r)
                              (debug-print "client -> server(property) ~a~%" data)
                              (let ((a (x:intern-atom-reply-atom r)))
                                (x:change-property client x:+prop-mode--append+ server-window a x:+atom--string+ 8 len data)
                                (send-client-message client server-window 32 (xim-atom-xim-protocol xim) (vector len a))
                                (x:flush client)))
                            0 (length name) name)
             (x:flush client))))))

(defun set-message-handler (xim code handler)
  (setf (aref (xim-message-handler-table xim) code) handler))

(defun read-message (xim e)
  (labels ((%read (data)
             (let* ((code (aref data 0))
                    (reader (aref *xim-reader-table* code))
                    (handler (aref (xim-message-handler-table xim) code)))
               (if reader
                   (if handler
                       (funcall handler (funcall reader data)))
                   (debug-print "undefined reader for ~a, data: ~a" code data)))))
    (let ((data (x:client-message-event-data e)))
      (case (x:client-message-event-format e)
        (8
         (debug-print "server -> client(client-message) ~a~%" data)
         (%read data))
        (32 (let ((len (aref data 0))
                  (prop-atom (aref data 1))
                  (client (xim-client xim)))
              (x:get-property client
                              (lambda (r)
                                (debug-print "server -> client(property) ~a~%" (x:get-property-reply-value r))
                                (%read (x:get-property-reply-value r)))
                              1 (xim-client-window xim) prop-atom x:+atom--string+
                              0 len)
              (x:flush client)))))))

(defun get-root (client)
  (x:screen-root (elt (x:setup-roots (x:client-server-information client)) 0)))

(defun xim-connect (xim)
  (let ((data (x:make-buffer 20)))
    (x:write-card8 data 0  +xim-connect+)
    (x:write-card16 data 2 2)
    (x:write-card8 data 4 #x6c) ; LSB
    (x:write-card16 data 6 0) ; major protocol version
    (x:write-card16 data 8 0) ; minor protocol version
    (send-message xim data)))

(defun read-xim-connect-reply (data)
  (list :server-major-protocol-version (x::read-card16 data 4)
        :server-minor-protocol-version (x::read-card16 data 6)))

(setf (aref *xim-reader-table* +xim-connect-reply+) #'read-xim-connect-reply)

(defun xim-disconnect (xim)
  (send-message xim (vector +xim-disconnect+)))

(defun read-xim-disconnect-reply (data)
  (declare (ignore data))
  nil)

(setf (aref *xim-reader-table* +xim-disconnect-reply+) #'read-xim-disconnect-reply)

(defun xim-open (xim)
  (let* ((locale (x:string->card8-vector (uiop:getenv "LANG")))
         (len (length locale))
         (data-byte-size (x::aligned-length (+ 1 len)))
         (data (x:make-buffer (+ 4 data-byte-size))))
    (x:write-card8 data 0 +xim-open+)
    (x:write-card16 data 2 (floor data-byte-size 4))
    (x:write-card8 data 4 len)
    (x::write-list-of-data data 5 locale 1)
    (send-message xim data)))

(defun read-xim-open-reply (data)
  (let* ((offset 4)
         input-method-id n im-attrs m ic-attrs)
    (setf input-method-id (x:read-card16 data offset))
    (incf offset 2)
    (setf n (x:read-card16 data offset))
    (incf offset 2)
    (labels ((read-attr (read-bytes bytes)
               (if (< read-bytes bytes)
                   (let (id type str-len str)
                     (setf id (x:read-card16 data offset))
                     (incf offset 2)
                     (setf type (x:read-card16 data offset))
                     (incf offset 2)
                     (setf str-len (x:read-card16 data offset))
                     (incf offset 2)
                     (setf str (make-string str-len))
                     (dotimes (i str-len)
                       (setf (aref str i) (code-char (x:read-card8 data offset)))
                       (incf offset 1))
                     (setf offset (x::aligned-length offset))
                     (cons (list :id id :type type :attribute str) (read-attr (+ read-bytes (x::aligned-length (+ 6 str-len))) bytes)))
                   nil)))
      (setf im-attrs (read-attr 0 n))
      (setf m (x:read-card16 data offset))
      (incf offset 4)
      (setf ic-attrs (read-attr 0 m))
      (list :input-method-id input-method-id
            :im-attributes im-attrs
            :ic-attributes ic-attrs))))

(setf (aref *xim-reader-table* +xim-open-reply+) #'read-xim-open-reply)

(defun xim-close (xim)
  (let ((data (x:make-buffer 20)))
    (x:write-card8 data 0 +xim-close+)
    (x:write-card16 data 2 1)
    (x:write-card16 data 4 (xim-input-method-id xim))
    (send-message xim data)))

(defun read-xim-close-reply (data)
  (list :input-method-id (x::read-card16 data 4)))

(setf (aref *xim-reader-table* +xim-close-reply+) #'read-xim-close-reply)

(defun read-xim-set-event-mask (data)
  (list :input-method-id (x::read-card16 data 4)
        :input-context-id (x::read-card16 data 6)
        :forward-event-mask (x::read-card32 data 8)
        :synchronous-event-mask (x::read-card32 data 12)))

(setf (aref *xim-reader-table* +xim-set-event-mask+) #'read-xim-set-event-mask)

(defun get-attribute-id (attrs name)
  (getf (find-if (lambda (attr) (equal (getf attr :attribute) name)) attrs) :id))

(defun xim-create-ic (xim)
  (let* ((ic-attrs (xim-ic-attributes xim))
         (input-style-id (get-attribute-id ic-attrs "inputStyle"))
         (client-window-id (get-attribute-id ic-attrs "clientWindow"))
         (focus-window-id (get-attribute-id ic-attrs "focusWindow"))
         (attrs-bytes (+ (if input-style-id 8 0)
                         (if client-window-id 8 0)
                         (if focus-window-id 8 0)))
         (data (x:make-buffer (+ 8 attrs-bytes)))
         (offset 8))
    (x:write-card8 data 0 +xim-create-ic+)
    (x:write-card16 data 2 (1+ (floor attrs-bytes 4)))
    (x:write-card16 data 4 (xim-input-method-id xim))
    (x:write-card16 data 6 attrs-bytes)
    (when input-style-id
      (x:write-card16 data offset input-style-id)
      (incf offset 2)
      (x:write-card16 data offset 4)
      (incf offset 2)
      (x:write-card32 data offset (logior #x8 #x400))
      (incf offset 4))
    (when client-window-id
      (x:write-card16 data offset client-window-id)
      (incf offset 2)
      (x:write-card16 data offset 4)
      (incf offset 2)
      (x:write-card32 data offset (xim-client-window xim))
      (incf offset 4))
    (when focus-window-id
      (x:write-card16 data offset focus-window-id)
      (incf offset 2)
      (x:write-card16 data offset 4)
      (incf offset 2)
      (x:write-card32 data offset (xim-client-window xim))
      (incf offset 4))
    (send-message xim data)))

(defun read-xim-create-ic-reply (data)
  (list :input-method-id (x::read-card16 data 4)
        :input-context-id (x::read-card16 data 6)))

(setf (aref *xim-reader-table* +xim-create-ic-reply+) #'read-xim-create-ic-reply)

(defmacro send-im-ic-id (code)
  `(let ((data (x:make-buffer 20)))
     (x:write-card8 data 0 ,code)
     (x:write-card16 data 2 1)
     (x:write-card16 data 4 (xim-input-method-id xim))
     (x:write-card16 data 6 (xim-input-context-id xim))
     (send-message xim data)))

(defun xim-set-ic-focus (xim)
  (send-im-ic-id +xim-set-ic-focus+))

(defun xim-unset-ic-focus (xim)
  (send-im-ic-id +xim-unset-ic-focus+))

(defun xim-forward-event (xim event)
  (let ((data (x:make-buffer 44)))
    (x:write-card8 data 0 +xim-forward-event+)
    (x:write-card16 data 2 10)
    (x:write-card16 data 4 (xim-input-method-id xim))
    (x:write-card16 data 6 (xim-input-context-id xim))
    (x:write-card16 data 8 0)
    (x:write-card16 data 10 (ldb (byte 16 16) (x::client-request-sequence-number (xim-client xim))))
    (funcall (aref x::*write-event-functions* (x::x-event-code event)) data (x:make-offset 12) event)
    (x:write-card16 data 14 (ldb (byte 16 0) (x::client-request-sequence-number (xim-client xim))))
    (send-message xim data)))

(defun read-xim-forward-event (data)
  (list :input-method-id (x::read-card16 data 4)
        :input-context-id (x::read-card16 data 6)
        :flag (x::read-card16 data 8)
        :serial-number (x::read-card16 data 10)
        :event (let ((code (x::read-card8 data 12)))
                 (funcall (aref x::*read-event-functions* code) data (x:make-offset 12)))))

(setf (aref *xim-reader-table* +xim-forward-event+) #'read-xim-forward-event)

(defun xim-sync (xim)
  (send-im-ic-id +xim-sync+))

(defun xim-sync-reply (xim )
  (send-im-ic-id +xim-sync-reply+))

(defun read-xim-commit (data)
  (let ((len (x::read-card16 data 10)))
    (list :input-method-id (x::read-card16 data 4)
          :input-context-id (x::read-card16 data 6)
          :flag (x::read-card16 data 8)
          :length len
          :string (let ((start (x::read-card8 data 12)))
                    (if (eql start #.(char-code #\esc))
                        (trivial-utf-8:utf-8-bytes-to-string data :start 15 :end (+ #.(- 12 3) len))
                        (trivial-utf-8:utf-8-bytes-to-string data :start 12 :end (+ 12 len)))))))

(setf (aref *xim-reader-table* +xim-commit+) #'read-xim-commit)

(defun client-message-handler (xim another-handler)
  (lambda (e)
    (let ((type (x:client-message-event-type e)))
      (cond
        ((eql type (xim-atom-xim-protocol xim))
         (read-message xim e))
        ((eql type (xim-atom-xim-xconnect xim))
         (setf (xim-server-window xim)
               (aref (x:client-message-event-data e) 0)))
        (another-handler
         (funcall another-handler e))))))

(defun loop-while (client cond)
  (when (funcall cond)
    (x:process-input client)
    (sleep 0.01)
    (loop-while client cond)))

(defun get-im-server-atom (xim)
  (let ((client (xim-client xim)))
    (elt (x:card8-vector->card32-vector
          (x:get-property-reply-value
           (x:get-property-sync client 0 (get-root client) (xim-atom-xim-servers xim)
                                x:+get-property-type--any+ 0 (1- (expt 2 32)))))
         0)))

(defun set-filter-event-handler (xim filter-event-cond code another-handler)
  (setf (aref (xim-filtered-event-handler-table xim) code) another-handler)
  (x:set-event-handler (xim-client xim) code
                       (lambda (e)
                         (if (funcall filter-event-cond e)
                             (xim-forward-event xim e)
                             (funcall another-handler e)))))

(defun create-xim (client window on-commit &optional client-message-handler)
  (let ((xim (make-xim)))
    (setf (xim-client xim) client
          (xim-atom-xim-servers xim) (intern-atom-sync client "XIM_SERVERS")
          (xim-atom-xim-xconnect xim) (intern-atom-sync client "_XIM_XCONNECT")
          (xim-atom-xim-moredata xim) (intern-atom-sync client "_XIM_MOREDATA")
          (xim-atom-xim-protocol xim) (intern-atom-sync client "_XIM_PROTOCOL")
          (xim-client-window xim) window
          (xim-im-server-atom xim) (get-im-server-atom xim)
          (xim-im-server-window xim) (x:get-selection-owner-reply-owner (x:get-selection-owner-sync client (xim-im-server-atom xim)))
          (xim-on-commit xim) on-commit)
    (x:set-event-handler client x:+client-message-event+ 
                         (client-message-handler xim client-message-handler))
    ;; pre connect
    (send-client-message client (xim-im-server-window xim)
                         32 (xim-atom-xim-xconnect xim)
                         (vector window 0 0)) ;; only-cm & property-with-cm
    (x:flush client)
    (loop-while client (lambda () (null (xim-server-window xim))))
    ;; connect
    (let (reply)
      (set-message-handler xim +xim-connect-reply+ (lambda (m) (setf reply m)))
      (xim-connect xim)
      (loop-while client (lambda () (null reply))))
    ;; open
    (set-message-handler xim +xim-open-reply+ (lambda (m)
                                                (setf (xim-input-method-id xim) (getf m :input-method-id)
                                                      (xim-im-attributes xim) (getf m :im-attributes)
                                                      (xim-ic-attributes xim ) (getf m :ic-attributes))))
    (xim-open xim)
    (loop-while client (lambda () (null (xim-input-method-id xim))))
    ;; create ic
    (set-message-handler xim +xim-create-ic-reply+ (lambda (m)
                                                     (setf (xim-input-context-id xim) (getf m :input-context-id))))
    (xim-create-ic xim)
    (loop-while client (lambda () (null (xim-input-context-id xim))))
    ;; set focus
    (xim-set-ic-focus xim)
    ;; forward event handler
    (set-message-handler xim +xim-forward-event+ (lambda (m)
                                                   (let* ((e (getf m :event))
                                                          (h (aref (xim-filtered-event-handler-table xim) (x::x-event-code e))))
                                                     (when h
                                                       (funcall h e)))
                                                   (xim-sync-reply xim)
                                                   (x:flush client)))
    ;; commit handler
    (set-message-handler xim +xim-commit+ (lambda (m)
                                            (let ((h (xim-on-commit xim)))
                                              (funcall h (getf m :string)))
                                            (xim-sync-reply xim)
                                            (x:flush client)))
    xim))

(defun destroy-xim (xim)
  (let (synced)
    (set-message-handler xim +xim-close-reply+ (lambda (m) (declare (ignore m)) (setf synced t)))
    (xim-close xim)
    (loop-while (xim-client xim) (lambda () (null synced))))
  (let (synced)
    (set-message-handler xim +xim-disconnect-reply+ (lambda (m) (declare (ignore m)) (setf synced t)))
    (xim-disconnect xim)
    (loop-while (xim-client xim) (lambda () (null synced)))))
