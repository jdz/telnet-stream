(in-package #:telnet-stream)

(define-condition read-timeout (serious-condition)
  ((stream
    :initarg :stream
    :reader read-timeout-stream))
  (:report (lambda (condition stream)
             (format stream "Timeout while waiting for input on ~S"
                     (read-timeout-stream condition)))))

(defgeneric process-iac (stream command option))
(defgeneric send-iac (stream command option))

(defclass telnet-stream (sb-gray:fundamental-character-input-stream
                         sb-gray:fundamental-character-output-stream)
  ((socket
    :initarg :socket
    :reader telnet-stream-socket)
   (input-buffer
    :initform (make-array 256 :element-type '(unsigned-byte 8))
    :reader input-buffer
    :type (simple-array (unsigned-byte 8) (*)))
   (input-buffer-index
    :initform 0
    :accessor input-buffer-index
    :type fixnum)
   (input-buffer-n-bytes
    :initform 0
    :accessor input-buffer-n-bytes
    :type fixnum)
   (timeout
    :initarg :timeout
    :initform nil
    :accessor timeout
    :type (or null number))
   #-(and)
   (negotiated-options
    ;; This would be used to implement STATUS option.
    :initform '()
    :accessor negotiated-options)))

(defmethod send-iac ((stream telnet-stream) (command symbol) (option symbol))
  (let ((command (ecase command
                   (:WILL +WILL+)
                   (:WONT +WONT+)
                   (:DO +DO+)
                   (:DONT +DONT+)))
        (option (or (car (rassoc option *known-options*))
                    (error "Unknown option: ~S" option)))
        (buffer (make-array 3 :element-type '(unsigned-byte 8))))
    (declare (dynamic-extent buffer))
    (setf (aref buffer 0) +IAC+
          (aref buffer 1) command
          (aref buffer 2) option)
    (assert (= 3 (socket-send (telnet-stream-socket stream) buffer 3)))))

(defmethod process-iac ((stream telnet-stream)
                        (command integer)
                        (option integer))
  (process-iac stream
               (or (cdr (assoc command *known-commands*))
                   (error "Unknown command code: ~S" command))
               (or (cdr (assoc option *known-options*))
                   (error "Unknown option code: ~S" option))))

(defmethod process-iac ((stream telnet-stream)
                        (command (eql :WILL))
                        (option symbol))
  "Default method that acknowledges all WILLs."
  (send-iac stream :DO option))

(defmethod process-iac ((stream telnet-stream)
                        (command (eql :WONT))
                        (option symbol))
  "Ignore WON'Ts"
  nil)

(defmethod process-iac ((stream telnet-stream)
                        (command (eql :DO))
                        (option symbol))
  "Default method that refuses to DO anything."
  (send-iac stream :WONT option))

(defmethod process-iac ((stream telnet-stream)
                        (command (eql :DONT))
                        (option symbol))
  "Ignore DON'Ts")

;;; XXX: These should be done in subclasses, but let's have them here
;;; for now.
(defmethod process-iac ((stream telnet-stream)
                        (command (eql :DO))
                        (option (eql :SUPPRESS-GO-AHEAD)))
  "Acknowledge request to SUPPRESS-GO-AHEAD."
  (send-iac stream :WILL option))

(defmethod process-iac ((stream telnet-stream)
                        (command (eql :DO))
                        (option (eql :ECHO)))
  "Acknowledge request to turn on ECHO."
  (send-iac stream :WILL option))

(defmethod process-iac ((stream telnet-stream)
                        (command (eql :DONT))
                        (option (eql :ECHO)))
  "Acknowledge request to turn off ECHO."
  (send-iac stream :WONT option))

(defmethod process-iac ((stream telnet-stream)
                        (command (eql :WILL))
                        (option (eql :STATUS)))
  "Refuse to negotiate STATUS."
  (send-iac stream :WONT option))

(declaim (inline input-available-p))
(defun input-available-p (stream)
  (< (input-buffer-index stream)
     (input-buffer-n-bytes stream)))

(defgeneric fill-input-buffer (stream)
  (:method ((stream telnet-stream))
    "Returns NIL on EOF."
    ;; XXX: Handle the case of IAC on a boundary.
    (with-accessors ((socket telnet-stream-socket)
                     (timeout timeout))
        stream
      (when timeout
        (loop
          (cond ((unix-simple-poll (socket-file-descriptor socket)
                                   :input
                                   (truncate (* timeout 1000)))
                 (return))
                (t
                 (cerror "Wait another ~D seconds."
                         (make-condition 'read-timeout :stream stream)
                         timeout)))))

      (multiple-value-bind (buf len peer)
          (socket-receive (telnet-stream-socket stream)
                          (input-buffer stream)
                          nil
                          :element-type '(unsigned-byte 8))
        (declare (ignorable buf peer))
        (setf (input-buffer-index stream) 0
              (input-buffer-n-bytes stream) len)
        (return-from fill-input-buffer (not (zerop len)))))))

(defun consume-iacs (stream)
  "Returns T if any IACs have been consumed."
  (with-accessors ((buffer input-buffer)
                   (index input-buffer-index)
                   (n-bytes input-buffer-n-bytes))
      stream
    (loop for n upfrom 0
          while (and (< index n-bytes)
                     (= +IAC+ (aref buffer index)))
          do (assert (< (+ index 2) n-bytes)
                     (stream index n-bytes)
                     "Proper buffer filling not implemented (IAC on boundary).")
             (process-iac stream
                          (aref buffer (+ index 1))
                          (aref buffer (+ index 2)))
             (incf index 3)
          finally ;; XXX: Force output if any IACs have been
                  ;; processed.
                  (return (not (zerop n))))))

;; (defmethod sb-gray:stream-clear-input ((stream telnet-stream))
;;   ;; XXX: Should do the Telnet EL (Erase Line)
;;   )

(defmethod sb-gray:stream-read-char ((stream telnet-stream))
  (with-accessors ((buffer input-buffer)
                   (index input-buffer-index)
                   (n-bytes input-buffer-n-bytes))
      stream
    (loop
      (unless (input-available-p stream)
        (unless (fill-input-buffer stream)
          (return-from sb-gray:stream-read-char :eof)))

      (unless (consume-iacs stream)
        (return-from sb-gray:stream-read-char
          (code-char (aref buffer (prog1 index (incf index)))))))))

;; (defmethod sb-gray:stream-read-sequence ((stream telnet-stream))
;;   )

;; (defmethod sb-gray:stream-peek-char ((stream telnet-stream))
;;   )

;; (defmethod sb-gray:stream-read-char-no-hang ((stream telnet-stream))
;;   )

;; (defmethod sb-gray:stream-read-line ((stream telnet-stream))
;;   )

(defmethod sb-gray:stream-listen ((stream telnet-stream))
  ;; XXX: Ensure this returns false on EOF.
  (or (input-available-p stream)
      (with-accessors ((socket telnet-stream-socket))
          stream
        (unix-simple-poll (socket-file-descriptor socket) :input 0))))

;; (defmethod sb-gray:stream-unread-char ((stream telnet-stream))
;;   )

;; (defmethod sb-gray:stream-clear-output ((stream telnet-stream))
;;   ;; Not essential.
;;   )

;; (defmethod sb-gray:stream-finish-output ((stream telnet-stream))
;;   )

;; (defmethod sb-gray:stream-force-output ((stream telnet-stream))
;;   )

;; (defmethod sb-gray:stream-write-sequence ((stream telnet-stream) seq
;;                                           &optional start end)
;;   )

;; (defmethod sb-gray:stream-advance-to-column ((stream telnet-stream)
;;                                              (column integer))
;;   )

;; (defmethod sb-gray:stream-fresh-line ((stream telnet-stream))
;;   )

;; (defmethod sb-gray:stream-line-column ((stream telnet-stream))
;;   )

;; (defmethod sb-gray:stream-line-length ((stream telnet-stream))
;;   )

;; (defmethod sb-gray:stream-start-line-p ((stream telnet-stream))
;;   )

;; (defmethod sb-gray:stream-terpri ((stream telnet-stream))
;;   )

(defmethod sb-gray:stream-write-char ((stream telnet-stream)
                                      (character character))
  (let ((buffer (make-array 1 :element-type '(unsigned-byte 8)))
        (code (char-code character)))
    (declare (dynamic-extent buffer))
    (check-type code (unsigned-byte 8))
    (setf (aref buffer 0) code)
    (assert (= 1 (socket-send (telnet-stream-socket stream) buffer 1)))))

;; (defmethod sb-gray:stream-write-string ((stream telnet-stream)
;;                                         (string string)
;;                                         &optional start end)
;;   )

