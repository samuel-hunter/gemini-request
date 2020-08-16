;;;; cl-gemini.asd

(asdf:defsystem #:cl-gemini
  :description "Communicate to servers through the gemini protocol"
  :author "Samuel Hunter"
  :license  "BSD 3-Clause"
  :version "0.0.1"
  :depends-on (#:alexandria
               #:cl+ssl
               #:puri
               #:trivial-sockets)

  :serial t
  :components ((:file "cl-gemini")))
