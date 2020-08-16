;;;; requests.lisp

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
   #:gmi-status
   #:gmi-category
   #:gmi-status=
   #:gmi-cat=

   ;; conditions
   #:gmi-error
   #:gmi-too-many-redirects
   #:gmi-code
   #:gmi-meta
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

(defvar *gemini-default-proxy* nil
  "The default proxy that a gemini request will go
through. NIL (default) means requests will not go through a proxy. See
GEMINI-REQUEST's PROXY argument for more documentation.")

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

(defun gmi-status (code)
  "Return a keyword representation of OBJ if it is (or has) a valid
Gemini response code; otherwise, return NIL."
  (and (< -1 code (length +response-codes+))
       (aref +response-codes+ code)))

(defun gmi-category (code)
  "Return a keyword representation of OBJ's category if it is (or
has) a valid Gemini response code; otherwise, return NIL."
  (and (gmi-status code)
       (aref +response-categories+ (floor code 10))))

(defun gmi-status= (status code)
  "Return whether the keyword STATUS describes CODE."
  (eq status (gmi-status code)))

(defun gmi-cat= (category code)
  "Return whether the keyword CATEGORY describes CODE."
  (eq category (gmi-category code)))

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
    (values (and (gmi-cat= :success code)
                 (read-stream-content-into-string stream))
            code
            meta)))

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
described in URI-STRING. This function returns THREE values - the body
of the response (which is NIL in non-successful responses), the status
code as an integer, and the response meta information.

VERIFY-SSL is passed on to CL+SSL:MAKE-SSL-CLIENT-STREAM as the VERIFY
argument. See *gemini-default-verify-ssl* for more documentation.

This alternative command does not resolve redirects, automatically
resolve the host or port to request from, or raise an error on
unsuccessful response codes. If you feel unsatisfied with how
GEMINI-REQUEST processes its input, use this instead for finer
control."
  (with-gemini-stream (gmi host port :ssl-options (:verify verify-ssl))
    (gemini-send-line uri-string gmi)
    (gemini-read-response gmi)))

(defun gemini-request (uri &key
                             (proxy *gemini-default-proxy*)
                             (verify-ssl *gemini-default-verify-ssl*)
                             (max-redirects 5)
                             (gemini-error-p t))
  "Sends a Gemini request to a server and returns its response. URI is
where the request is sent to, and can either be a string, or a
PURI:URI object. This function returns FOUR values - the body of the
response (which is NIL in non-successful responses), the status code
as an integer, the response meta information (which would be the MIME
media type in successful responses), and finally the URI the reply
comes from (which might be different from the URI the request was sent
to in case of redirects).

If PROXY is not NIL, it should be either a string denoting a proxy
server through which the request should be sent, or a cons pair
of (HOST-STRING PORT-NUMBER). Defaults to *gemini-default-proxy-host*.

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
  ;; Configure parameters.
  (setf uri
        (etypecase uri
          (string (puri:parse-uri uri))
          (puri:uri (puri:copy-uri uri))))

  (etypecase proxy
    (null (setf proxy (cons (puri:uri-host uri)
                            +gemini-default-port+)))
    (string (setf proxy (cons proxy +gemini-default-port+)))
    (cons))

  (loop :with redirect-trace := ()
        :with uri-string := (puri:render-uri uri nil)
        :for (body code meta) := (multiple-value-list (gemini-request* uri-string
                                                                       (car proxy) (cdr proxy)
                                                                       verify-ssl))
        :for redirects :upfrom 0
        :do (ecase (gmi-category code)
              ;; any non-exceptional categories
              ((:success
                :input)
               (return (values body code meta uri-string)))

              ;; continue looping with a new uri
              (:redirect
               (unless (minusp max-redirects)
                 (push (cons (gmi-status code) meta) redirect-trace))
               (setf uri (puri:parse-uri meta)))

              ;; failure
              ((:temporary-failure
                :permanent-failure
                :client-certificate-required
                nil)
               (if gemini-error-p
                   (error 'gmi-error :code code :meta meta)
                   (return (values body code meta uri-string)))))
        :when (= redirects max-redirects)
          :do (error 'gmi-too-many-redirects
                     :code code :meta meta
                     :redirect-trace redirect-trace)))
