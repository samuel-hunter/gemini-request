;;;; cl-gemini.asd

(asdf:defsystem #:cl-gemini
  :description "Describe cl-gemini here"
  :author "Samuel Hunter"
  :license  "BSD 3-Clause"
  :version "0.0.1"
  :depends-on (#:alexandria
               #:cl+ssl
               #:trivial-sockets)

  :serial t
  :components ((:file "cl-gemini")))
