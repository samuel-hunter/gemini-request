;;;; cl-gemini.lisp

(defpackage #:cl-gemini
  (:nicknames :gemini :gmi)
  (:use #:cl #:alexandria)
  (:export #:+gemini-version+
           #:fetch-url))

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
       ,@(loop :for (code-name value) :in code-forms
               :for var-symbol := (intern (format nil "+CODE-~A+" code-name))
               :collect `(defparameter ,var-symbol ,value))
       (defparameter ,table-var (make-array ,(1+ largest-code)
                                            :element-type 'keyword))
       ,@(loop :for (code-name value) :in code-forms
               :for keyword := (intern (string code-name) #.(find-package "KEYWORD"))
               :collect `(setf (aref ,table-var ,value) ,keyword)))))

;; Status codes, found in Appendix 1.
(define-codes +response-codes+
  (#:input 10)
  (#:sensitive-input 11)

  (#:success 20)

  (#:redirect 30)
  (#:redirect-temporary 30)
  (#:redirect-permanent 31)

  (#:temporary-failure 40)
  (#:server-unavailable 41)
  (#:cgi-error 42)
  (#:proxy-error 43)
  (#:slow-down 44)

  (#:permanent-failure 50)
  (#:not-found 51) ;; You can't find things hidden in Area 51!
  (#:gone 52)
  (#:proxy-request-refused 53)
  (#:bad-request 59)

  (#:client-certificate-required 60)
  (#:certificate-not-authorized 61)
  (#:certificate-not-valid 62))

(defstruct response
  code meta body)

(defun fetch-url (url &optional proxy (max-retries 0))
  (declare (ignore url proxy max-retries))
  (make-response :code +code-success+ :body "Hello, World!"))
