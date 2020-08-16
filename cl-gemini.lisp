;;;; cl-gemini.lisp

(defpackage #:cl-gemini
  (:nicknames :gemini :gmi)
  (:use #:cl #:alexandria)
  (:export
   ;; Constants and dynamic vars
   #:+gemini-version+
   #:+gemini-default-port+
   #:*gemini-default-proxy-host*
   #:*gemini-default-proxy-port*
   #:*gemini-default-verify-ssl*

   ;; response code
   #:gmi-status
   #:gmi-category
   #:gmi-status=
   #:gmi-cat=

   ;; response data
   #:gmi-response
   #:gmi-code
   #:gmi-meta
   #:gmi-body

   ;; conditions
   #:gmi-error
   #:gmi-too-many-redirects
   #:gmi-redirect-trace

   ;; request
   #:gemini-request*
   #:gemini-request))

(in-package #:cl-gemini)

;; Direct quote from
;; gemini://gemini.circumlunar.space/docs/specification.gmi
;; as of 2020-07-02 (visited 2020-08-10):

;; This is an increasingly less rough sketch of an actual spec for
;; Project Gemini.  Although not finalised yet, further changes to the
;; specification are likely to be relatively small.  You can write
;; code to this pseudo-specification and be confident that it probably
;; won't become totally non-functional due to massive changes next
;; week, but you are still urged to keep an eye on ongoing development
;; of the protocol and make changes as required.
(defvar +gemini-version+ '(0 14 2)
  "The major version, minor version, and patchlevel of the
library's supported gemini protocol.")

(defvar +gemini-default-port+ 1965
  "The default port for a gemini server.")

(defvar *gemini-default-proxy-host* nil
  "The default proxy that a gemini request will go
through. NIL (default) means requests will not go through a proxy.")

(defvar *gemini-default-proxy-port* nil
  "The default proxy port that a gemini request will go through if
*gemini-default-proxy-host* is bound. NIL (default) means requests go
through the default Gemini port.")

(defvar *gemini-default-verify-ssl* :optional
  "The default choice for verifying an SSL certificate. It has the
same choices as the CL+SSL:MAKE-SSL-CLIENT-STREAM's :VERIFY keyword
argument - that is, NIL means to not verify, :OPTIONAL (default) means
to verify if the certificate is provided, and T means to always
verify.")

(defmacro define-codes (table-var &body code-forms)
  (let ((largest-code (reduce #'max (mapcar #'cadr code-forms))))
    `(progn
       (defparameter ,table-var (make-array ,(1+ largest-code)
                                            :element-type '(or keyword null)
                                            :initial-element nil))
       ,@(loop :for (keyword value) :in code-forms
               :collect `(setf (aref ,table-var ,value) ,keyword)))))

;; Status codes, found in Appendix 1.
(define-codes +response-codes+
  (:input 10)
  (:sensitive-input 11)

  (:success 20)

  (:redirect-temporary 30)
  (:redirect-permanent 31)

  (:temporary-failure 40)
  (:server-unavailable 41)
  (:cgi-error 42)
  (:proxy-error 43)
  (:slow-down 44)

  (:permanent-failure 50)
  (:not-found 51) ;; You can't find things hidden in Area 51!
  (:gone 52)
  (:proxy-request-refused 53)
  (:bad-request 59)

  (:client-certificate-required 60)
  (:certificate-not-authorized 61)
  (:certificate-not-valid 62))

;; Status categories, bound to the first digit.
(define-codes +response-categories+
  (:input 1)
  (:success 2)
  (:redirect 3)
  (:temporary-failure 4)
  (:permanent-failure 5)
  (:client-certificate-required 6))

(defgeneric gmi-status (obj)
  (:documentation
   "Return a keyword representation of OBJ if it is (or has) a valid
Gemini response code; otherwise, return NIL.")
  (:method ((obj integer))
    (and (< -1 obj (length +response-codes+))
         (aref +response-codes+ obj))))

(defgeneric gmi-category (obj)
  (:documentation
   "Return a keyword representation of OBJ's category if it is (or
has) a valid Gemini response code; otherwise, return NIL.")
  (:method ((obj integer))
    (let ((category (floor obj 10)))
      (and (< -1 category (length +response-categories+))
           (aref +response-categories+ category)))))

(defun gmi-status= (status obj)
  "Return whether the keyword STATUS describes OBJ's response code."
  (eq status (gmi-status obj)))

(defun gmi-cat= (category obj)
  "Return whether the keyword CATEGORY describes OBJ's response category."
  (eq category (gmi-category obj)))

;; response data

(defclass gmi-response ()
  ((code :type integer :initarg :code :reader gmi-code)
   (meta :type string :initarg :meta :reader gmi-meta)
   (body :type (or string null) :initarg :body :reader gmi-body)))

(defmethod print-object ((obj gmi-response) stream)
  (with-slots (code meta body) obj
    (format stream "#<~S CODE=~A (~A) META=~S~@[ BODY=~S~]>"
            'gmi-response
            code (gmi-status code)
            meta
            body)))

(defmethod gmi-status ((obj gmi-response))
  (gmi-status (gmi-code obj)))

(defmethod gmi-category ((obj gmi-response))
  (gmi-category (gmi-code obj)))

;; Conditions

(define-condition gmi-error (error)
  ((code :initarg :code :reader gmi-code)
   (meta :initarg :meta :reader gmi-meta))
  (:report (lambda (condition stream)
             (with-slots (code meta) condition
               (format stream "Response returned code ~D (~A): ~A"
                       code (or (gmi-status code) :unknown) meta)))))

(define-condition gmi-too-many-redirects (gmi-error)
  ((redirect-trace :initarg :redirect-trace :reader gmi-redirect-trace))
  (:report (lambda (condition stream)
             (format stream "Too many redirects. Trace: ~S"
                     (slot-value condition 'redirect-trace)))))

;; Write a request

(defun gemini-send-line (line stream)
  "Send a single line to a gemini stream, print CRLF, and force output."
  (format stream "~A~C~C"
          line
          #\Return #\Linefeed)
  (force-output stream))

;; Read a resposne

(defun read-line-crlf (stream)
  (with-output-to-string (out)
    (loop
      :for chr := (read-char stream nil)
      :while (and chr (not (char= chr #\Return)))
      :do (write-char chr out)
          ;; consume linefeed
      :finally (read-char stream nil))))

(defun parse-response-header (response)
  "Return two values, the response code as a keyword, and the response meta."
  (multiple-value-bind (code start)
      (parse-integer response :junk-allowed t)

    (with-input-from-string (s (subseq response start))
      (peek-char t s)  ;; skip over whitespace
      (values code
              (read-stream-content-into-string s)))))

(defun gemini-read-response (stream)
  "Consume all data from STREAM and return a structured response."
  (multiple-value-bind (code meta)
      (parse-response-header (read-line-crlf stream))
    (make-instance 'gmi-response
                   :code code
                   :meta meta
                   :body (and (gmi-cat= :success code)
                             (read-stream-content-into-string stream)))))

;; Set up the stream
(defmacro with-gemini-stream ((var server port &key ssl-options) &body body)
  (with-unique-names (socket)
    `(let ((,socket (trivial-sockets:open-stream ,server ,port)))
       (with-open-stream (,var (cl+ssl:make-ssl-client-stream
                                ,socket
                                :unwrap-stream-p t
                                :external-format '(:utf-8 :eol-style :lf)
                                ,@ssl-options))
         ,@body))))

;; Put it together

(defun gemini-request* (uri-string host port verify-ssl)
  "Sends a Gemini request to HOST:PORT asking for the resource
URI-STRING. VERIFY-SSL is passed on to CL+SSL:MAKE-SSL-CLIENT-STREAM
as the VERIFY argument. See *gemini-default-verify-ssl* for more
documentation.

This alternative command does not resolve redirects, automatically
resolve the host or port to request from, or raise an error on
unsuccessful response codes. If you feel unsatisfied with how
GEMINI-REQUEST processes its input, use this instead for finer
control."
  (with-gemini-stream (gmi host port :ssl-options (:verify verify-ssl))
    (gemini-send-line uri-string gmi)
    (gemini-read-response gmi)))

(defun uri-host (uri-string)
  (puri:uri-host (puri:parse-uri uri-string)))

(defun uri-port (uri-string)
  (puri:uri-port (puri:parse-uri uri-string)))

(defmacro gemini-error (error-class response &rest misc-args)
  (once-only (response)
    `(error ,error-class
            :code (gmi-code ,response)
            :meta (gmi-meta ,response)
            ,@misc-args)))

(defun gemini-request (uri-string &key
                                    (proxy-host (or *gemini-default-proxy-host*
                                                    (uri-host uri-string)))
                                    proxy-port
                                    (verify-ssl *gemini-default-verify-ssl*)
                                    (max-redirects 5)
                                    (gemini-error-p t))
  "Sends a Gemini request to a server and returns its reply as a
GMI-RESPONSE object. URI-STRING is where the request is sent to.

If PROXY-HOST is not NIL, it should be a string denoting a proxy
server through which the request should be sent. Defaults to
*gemini-default-proxy-host*.

VERIFY-SSL is passed on to CL+SSL:MAKE-SSL-CLIENT-STREAM as the VERIFY
argument. Defaults to *gemini-default-verify-ssl*. See
*gemini-default-verify-ssl* for more documentation.

MAX-REDIRECTS is the maximum number of redirects that will be
processed before signaling a GMI-TOO-MANY-REDIRECTS error. Negative
values signal that it should process an infinite number of
redirects (not recommended). Defaults to 5.

GEMINI-ERROR-P denotes whether to signal a GMI-ERROR instead of
returning a response when its code is 4x (TEMPORARY FAILURE),
5x (PERMANENT FAILURE), 6x (CLIENT CERTIFICATE REQUIRED), or an
unknown respone code. It does not influence raising an error for too
many requests. Defaults to T."
  ;; Set the proxy port if none is set yet. I can't do this in the
  ;; function header because it needs to know the value of PROXY-HOST
  ;; to decide the default value.
  (unless proxy-port
    (setf proxy-port (or (if proxy-host
                             *gemini-default-proxy-port*
                             (uri-port uri-string))
                         +gemini-default-port+)))

  (loop :with redirect-trace := ()
        :for response := (gemini-request* uri-string proxy-host proxy-port verify-ssl)
        :for redirects :upfrom 0
        :do (ecase (gmi-category response)
              ;; any non-exceptional categories
              ((:success
                :input)
               (return response))

              ;; continue looping with a new uri
              (:redirect
               (with-slots (code meta) response
                 (unless (minusp max-redirects)
                   (push (cons (gmi-status code) meta) redirect-trace))
                 (setf uri-string meta)))

              ;; failure
              ((:temporary-failure
                :permanent-failure
                :client-certificate-required
                nil)
               (if gemini-error-p
                   (gemini-error 'gmi-error response)
                   (return response))))
        :when (= redirects max-redirects)
          :do (gemini-error 'gmi-too-many-redirects response
                         :redirect-trace redirect-trace)))
