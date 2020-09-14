;;;; requests.lisp

(defpackage #:gemini-request
  (:use #:cl #:alexandria)
  (:export
   ;; Constants and dynamic vars
   #:+gemini-version+
   #:+gemini-default-port+
   #:*gemini-default-proxy*

   ;; response code
   #:gmi-status
   #:gmi-category
   #:gmi-status=
   #:gmi-cat=
   #:gmi-success-p

   ;; conditions
   #:gmi-error
   #:gmi-too-many-redirects
   #:gmi-redirect-trace
   #:gmi-misbehaving-server-error
   #:gmi-reason

   #:gmi-warning
   #:gmi-unknown-charset-warning
   #:gmi-charset

   ;; request
   #:gemini-request-stream*
   #:gemini-request-stream
   #:gemini-request))

(in-package #:gemini-request)

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

(defun gmi-success-p (code)
  "Return whether CODE is in the successful category."
  (gmi-cat= :success code))

;; Conditions

(define-condition gmi-error (error) ())

(define-condition gmi-too-many-redirects (gmi-error)
  ((redirect-trace :initarg :redirect-trace :reader gmi-redirect-trace))
  (:report (lambda (condition stream)
             (format stream "Too many redirects. Trace: ~S"
                     (slot-value condition 'redirect-trace)))))

(define-condition gmi-misbehaving-server-error (gmi-error)
  ((reason :initarg :reason :type string :reader gmi-reason))
  (:report (lambda (condition stream)
             (princ (slot-value condition 'reason)
                    stream))))

(define-condition gmi-warning (warning) ())

(define-condition gmi-unknown-charset-warning (gmi-warning)
  ((charset :initarg :charset :type string :reader gmi-charset))
  (:report (lambda (condition stream)
             (format stream "~S is not known to be a name for an external format."
                     (slot-value condition 'charset)))))

;; Read a response

(defparameter +whitespace-bag+ #(#\Space #\Tab)
  "Character bag for whitespace characters")

(defun read-to (char-bag stream)
  "Read until the (consumed) delimiter or EOF is reached."
  (with-output-to-string (out)
    (loop :for c := (read-char stream nil)
          :until (or (null c) (position c char-bag))
          :do (write-char c out))))

(defun split-code-and-meta (response)
  "Return two values from RESPONSE, an integer response code and a meta string."
  (with-input-from-string (s response)
    (handler-case
        (let ((code (parse-integer (read-to +whitespace-bag+ s))))
          (peek-char t s nil) ;; skip whitespace
          (values code (read-line s nil "")))
      (parse-error ()
        ;; `parse-integer' failed, so let's replace the parse-error
        ;; with a specific gmi-error instead.
        (error 'gmi-misbehaving-server-error
               :reason "Response header didn't start with a status code")))))

(defun parse-mimetype-and-params (meta)
  (with-input-from-string (stream meta)
    (loop :with mimetype := (read-to #(#\;) stream)
          :for next-char := (peek-char t stream nil)
          :while next-char
          ;; key=value pairs are collected into an alist, and
          ;; downcased as parameters and contents are case-insensitive.

          ;; NOTE: Should I filter out unsupported parameters right
          ;; here? I was thinking about replacing supported param keys
          ;; with keywords, but that would make users check if a key
          ;; is a keyword or string. Filtering only supported keywords
          ;; right here would knock two birds with one stone...
          :collect (cons (string-downcase (read-to #(#\=) stream))
                         (string-downcase (read-to #(#\;) stream)))
            :into params
          :finally (return (values mimetype params)))))

(defun gemini-read-response-header (stream)
  "Read a single line from STREAM and return four values: the code of
the response, the meta tag of the response, the mimetype of the
response (if the code is OK, otherwise NIL), and an alist of all
parameters of the response (if the code is OK, otherwise NIL)."
  (multiple-value-bind (code meta) (split-code-and-meta (read-line stream))
    (multiple-value-bind (mimetype params)
        (if (gmi-success-p code)
            (parse-mimetype-and-params meta)
            (values nil nil))
      (values code meta
              (if (string= "" mimetype)
                  ;; Unspecified mimetypes are de-facto text/gemini.
                  "text/gemini"
                  mimetype)
              params))))

;; Set up the stream
(defun gemini-connect (server port &optional ssl-options)
  "Connect to a Gemini server at SERVER:PORT and return an open stream
with SSL-OPTIONS passed to CL+SSL:MAKE-SSL-CLIENT-STREAM, aside from
EXTERNAL-FORMAT."
  (let ((socket (trivial-sockets:open-stream server port)))
    (apply #'cl+ssl:make-ssl-client-stream socket
           :external-format '(:utf-8 :eol-style :crlf)
           ssl-options)))

;; Put it together

(defun make-external-format-or-nil (name &rest args &key &allow-other-keys)
  "Return external formats if it doesn't raise an
external-format-error; else, return NIL."
  (handler-case
      (apply #'flex:make-external-format name args)
    (flex:external-format-error () nil)))

(defun gemini-request-stream* (uri-string host port &optional ssl-options)
  "Request the resource at URI-STRING at the connection HOST:PORT and
return six values: the stream of the response body; the response code
integer; the response meta string; the response mimetype string if
code is successful, else NIL; and a string alist of the response
parameters if code is successful, else NIL.

SSL-OPTIONS is a key-value plist of arguments passed to
CL+SSL:MAKE-SSL-CLIENT-STREAM.  All key options can be passed in
except for EXTERNAL-FORMAT. One option of note is VERIFY, which can be
used to specify whether the server certificate should be
verified (options are NIL, :optional, and :required). The default is
CL+SSL:*MAKE-SSL-CLIENT-STREAM-VERIFY-DEFAULT*, which is initialized
to :required."
  (let ((stream (gemini-connect host port ssl-options)))
    ;; Send request header
    (format stream "~A~%" uri-string)
    (force-output stream)

    ;; Read the response header
    (multiple-value-bind (code meta mimetype params)
        (gemini-read-response-header stream)

      ;; change the charset when necessary
      (if (gmi-success-p code)
        (let ((charset (cdr (assoc "charset" params :test #'string=))))
          (cond
            ;; change the flexi-stream's external format to the
            ;; response body's format if specified. Raise a
            ;; gmi-unknown-charset-warning if the external format
            ;; isn't supported by flexi-streams.
            (charset (if-let ((format (make-external-format-or-nil
                                       charset :eol-style :crlf)))
                       (setf (flex:flexi-stream-external-format stream) format)
                       (warn 'gmi-unknown-charset-warning :charset charset)))
            ;; with a text/* or default (text/gemini) mimetype, keep
            ;; the stream the same (utf-8)
            ((starts-with-subseq "text/" mimetype)
             (values)))))

      (values stream code meta mimetype params))))

(defun gemini-request-stream (uri &key
                                    (proxy *gemini-default-proxy*)
                                    (max-redirects 5)
                                    ssl-options)
  "Request the resource at URI, automatically redirect when required,
and return five values: the stream of the response body; the response
code integer; the response meta string; the response mimetype string
if code is successful, else NIL; a string alist of the response
parameters if code is successful, else NIL; and the URI of the
resolved resource, for when redirects have changed it.

If PROXY is not NIL, it should be either a string denoting a proxy
server through which the request should be sent, or a cons pair
of (HOST-STRING PORT-NUMBER). Defaults to *gemini-default-proxy-host*.

MAX-REDIRECTS is the maximum number of redirects that will be
processed before signaling a GMI-TOO-MANY-REDIRECTS error. Negative
values signal that it should process an infinite number of
redirects (not recommended). Defaults to 5.

SSL-OPTIONS is a key-value plist of arguments passed to
CL+SSL:MAKE-SSL-CLIENT-STREAM.  All key options can be passed in
except for EXTERNAL-FORMAT. One option of note is VERIFY, which can be
used to specify whether the server certificate should be
verified (options are NIL, :optional, and :required). The default is
CL+SSL:*MAKE-SSL-CLIENT-STREAM-VERIFY-DEFAULT*, which is initialized
to :required."
  ;; Configure parameters.
  (setf uri
        (etypecase uri
          (string (puri:parse-uri uri))
          (puri:uri uri)))

  (setf proxy
        (etypecase proxy
          (null (cons (puri:uri-host uri)
                      +gemini-default-port+))
          (string (cons proxy +gemini-default-port+))
          (cons proxy)))

  ;; TODO figure out how to handle redirects that change the URI
  ;; scheme. Maybe raise a condition and see how it goes from there?
  ;; Or rise an error when it happens and provide restarts like
  ;; `redirect-anyway'.
  (loop
    :with redirect-trace := ()
    :for redirects :upfrom 0
    :for uri-string := (puri:render-uri uri nil)
    :do
       (multiple-value-bind (stream code meta mimetype params)
           (gemini-request-stream* uri-string
                                   (car proxy) (cdr proxy)
                                   ssl-options)
         (if (gmi-cat= :redirect code)
             (progn
               (unless (minusp max-redirects)
                 (push (cons (gmi-status code) meta) redirect-trace))
               (setf uri (puri:merge-uris meta uri))
               (close stream))
             (return (values stream code meta mimetype params uri)))
         (close stream))
    :when (= redirects max-redirects)
      :do (error 'gmi-too-many-redirects
                 :redirect-trace redirect-trace)))

(defun gemini-request (uri &key
                             (proxy *gemini-default-proxy*)
                             (max-redirects 5)
                             ssl-options)
  "Request the resource at URI, automatically redirect when required,
and return five values: the response body on a successful response,
else NIL; the response code integer; the response meta string; the
response mimetype string if code is successful, else NIL; a string
alist of the response parameters if code is successful, else NIL; and
the URI of the resolved resource, for when redirects have changed it.

If PROXY is not NIL, it should be either a string denoting a proxy
server through which the request should be sent, or a cons pair
of (HOST-STRING PORT-NUMBER). Defaults to *gemini-default-proxy-host*.

MAX-REDIRECTS is the maximum number of redirects that will be
processed before signaling a GMI-TOO-MANY-REDIRECTS error. Negative
values signal that it should process an infinite number of
redirects (not recommended). Defaults to 5.

SSL-OPTIONS is a key-value plist of arguments passed to
CL+SSL:MAKE-SSL-CLIENT-STREAM.  All key options can be passed in
except for EXTERNAL-FORMAT. One option of note is VERIFY, which can be
used to specify whether the server certificate should be
verified (options are NIL, :optional, and :required). The default is
CL+SSL:*MAKE-SSL-CLIENT-STREAM-VERIFY-DEFAULT*, which is initialized
to :required."
  (multiple-value-bind (stream code meta mimetype params resolved-uri)
      (gemini-request-stream uri :proxy proxy
                                 :max-redirects max-redirects
                                 :ssl-options ssl-options)
    (values (cond
              ((not (gmi-success-p code)) nil)
              ((typep stream 'flex:flexi-stream)
               (read-stream-content-into-string stream))
              (t (read-stream-content-into-byte-vector stream)))
            code meta mimetype params resolved-uri)))
