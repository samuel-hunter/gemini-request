;;;; cl-gemini.lisp

(defpackage #:cl-gemini
  (:nicknames :gemini :gmi)
  (:use #:cl #:alexandria)
  (:export #:+gemini-version+

           ;; status code
           #:status-keyword

           ;; response
           #:resposne
           #:make-response
           #:response-code
           #:response-status
           #:resposne-category
           #:response-meta
           #:response-body

           ;; fetching
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
  "The major version, minor version, and patchlevel of the latest-supported gemini protocol.")

(defmacro define-codes (table-var &body code-forms)
  (let ((largest-code (reduce #'max (mapcar #'cadr code-forms))))
    `(progn
       (defparameter ,table-var (make-array ,(1+ largest-code)
                                            :element-type 'keyword
                                            :initial-element :undefined))
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

(defun status-keyword (code)
  (aref +response-codes+ code))

(defun code-successp (code)
  (= +code-success+ code))

(defstruct response
  code meta body)

(defun response-status (response)
  "Return the status keyword of RESPONSE."
  (status-keyword (response-code response)))

(defun code-category (code)
  (status-keyword (* (floor code 10) 10)))

(defun response-category (response)
  "Return the keyword of the RESPONSE's code category."
  (code-category (response-code response)))

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

(defun send-request (url stream)
  "Send a single-line URL request to an SSL stream."
  (format stream "~A~C~C"
          url #\Return #\Linefeed)
  (force-output stream))

(defun read-response (stream)
  "Consume all data from STREAM and return a structured response."
  (multiple-value-bind (code meta)
      (parse-response-header (read-line-crlf stream))
    (make-response :code code
                   :meta meta
                   :body (if (code-successp code)
                             (read-stream-content-into-string stream)
                             ""))))

;; TODO support 1x INPUT response codes
(defun fetch-url (url server &key (port 1965)
                               (verify-ssl cl+ssl:*make-ssl-client-stream-verify-default*))
  (with-open-stream (socket (trivial-sockets:open-stream server port))
    (let* ((ssl (cl+ssl:make-ssl-client-stream
                 socket
                 :unwrap-stream-p t
                 :external-format '(:utf-8 :eol-style :lf)
                 :verify verify-ssl)))
      (send-request url ssl)
      (read-response ssl))))
