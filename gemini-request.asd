;;;; gemini-request.asd

(asdf:defsystem #:gemini-request
  :description "Communicate to servers through the gemini protocol"
  :author "Samuel Hunter"
  :license  "BSD 3-Clause"
  :version "0.1.1"
  :depends-on (#:alexandria
               #:cl+ssl
               #:flexi-streams
               #:puri
               #:trivial-sockets)
  :serial t
  :components ((:file "request")))
