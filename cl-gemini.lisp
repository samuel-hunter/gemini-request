;;;; cl-gemini.lisp

(defpackage #:cl-gemini
  (:nicknames :gemini :gmi)
  (:use #:cl #:alexandria)
  (:export
   ;; Constants and dynamic vars
   #:+gemini-version+
   #:+gemini-default-port+
   #:*gemini-default-proxy*
   #:*gemini-default-verify-ssl*

   ;; response code
   #:gemini-status
   #:gemini-successp

   ;; response data
   #:response
   #:make-response
   #:response-code
   #:response-status
   #:response-category
   #:response-meta
   #:response-body

   ;; low-level request (for finer control)
   #:gemini-send-line
   #:gemini-read-response
   #:with-gemini-stream

   ;; high-level request
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
latest-supported gemini protocol.")

(defvar +gemini-default-port+ 1965
  "The default port for a gemini server.")

(defvar *gemini-default-proxy* nil
  "The default proxy that a gemini request will go
through. NIL (default) means requests will not go through a proxy.")

(defvar *gemini-default-verify-ssl* :optional
  "The default choice for verifying an SSL certificate. It has the
same choices as cl+ssl's :verify key - that is, NIL means to not
verify, :OPTIONAL (default) means to verify if the certificate is
provided, and T means to always verify.")

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

  (:redirect 30)
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

(defun gemini-status (code)
  "Return a keyword representation of CODE if it's a valid Gemini
response code; otherwise, return NIL."
  (and (< -1 code (length +response-codes+))
      (aref +response-codes+ code)))

(defun gemini-successp (code)
  "Return whether CODE describes a successful response."
  (eq :success (gemini-status code)))

;; response data

(defstruct response
  code meta body)

(defun response-status (response)
  "Return the status keyword of RESPONSE."
  (gemini-status (response-code response)))

(defun code-category (code)
  (gemini-status (* (floor code 10) 10)))

(defun response-category (response)
  "Return the keyword of the RESPONSE's code category."
  (code-category (response-code response)))

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
    (make-response :code code
                   :meta meta
                   :body (if (gemini-successp code)
                             (read-stream-content-into-string stream)
                             ""))))

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
(defun gemini-request* (url-string host port verify-ssl)
  (with-gemini-stream (gmi host port :ssl-options (:verify verify-ssl))
    (gemini-send-line url-string gmi)
    (gemini-read-response gmi)))

;; TODO support 1x INPUT response codes
;; TODO support redirects
(defun gemini-request (url-string proxy &key
                                    ;; (proxy (or *gemini-default-proxy*
                                    ;;            (purl:url-host url-string)))
                                    (port +gemini-default-port+)
                                    (verify-ssl *gemini-default-verify-ssl*))
  (gemini-request* url-string proxy port verify-ssl))
