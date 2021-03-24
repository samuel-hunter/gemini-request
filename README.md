# gemini-request

A library for communication to servers through the Gemini protocol.

**NOTE:** gemini-request (and the main project
[retrorocket](https://sr.ht/~shunter/retrorocket/)) has been defunct
since November 29, 2020 (Gemini v0.14.3). This project was not meant
to be a serious, maintained library, but rather to help in developing
Retrorocket to see if the UI toolkit McCLIM is a good match for me.

---

Project Gemini is a semi-recent internet protocol project that
describes itself as somewhere between Gopher and the conventional
Web. Instead of being the simplest protocol, Gemini intends to have
the highest "oomph" for its weight. If you're unfamiliar with Project
Gemini and want to learn more, I recommend reading the de-facto
homepage in [HTTP](https://gemini.circumlunar.space/) or in
[Gemini](gemini://gemini.circumlunar.space/) [(Mozz.us
Proxy)](https://portal.mozz.us/gemini/gemini.circumlunar.space/).

This library could be used for making Gemini-compliant browsers or
violated requests. While the Gemini protocol was simple enough I could
hack together this ad-hoc library in 3 days, it's still has a new
library smell, and bound to be full of bugs. Feel free to issue a
report for behavioral bugs, incorrect/unclear documentation, or even
reach out to recommend anything you think would improve it!

## Quick Usage

Drop this project in a place that ASDF can see it - I personally use
`~/quicklisp/local-projects/` so that quicklisp pulls in its
dependencies, `alexandria`, `cl+ssl`, `puri`, and `trivial-sockets`.

```lisp
* (require :gemini-request)

;; Make a simple gemini request. It returns the response body,
;; code, meta text, mime and parameter alist for successful requests,
;; and the resolved uri (say, if #'gemini-request handles redirects):
* (gemini-request:gemini-request "gemini://gemini.circumlunar.space")
"# Project Gemini

## Overview

Gemini is a new internet protocol which:

* Is heavier than gopher
* Is lighter than th..."
20
"text/gemini"
"text/gemini"
()
#<PURI:URI //gemini.circumlunar.space/>
```

## Extended Usage

```lisp
(ql:quickload :gemini-request)
(use-package #:gemini-request)


;; You can use puri:uri objects as well.
* (gemini-request (make-instance 'puri:uri
                                 :host "example.com"
                                 :path "about"))

;; You can configure the maximum redirects to automatically follow
;; (default is 5).
* (handler-case
      (gemini-request "//example.com" :max-redirects 3)
    (gmi-too-many-requests (cond)
      (format t "Too many requests!~%~S~%Goodbye.~%"
              (gmi-redirect-trace cond)))
Too many requests!
((:redirect-permanent . "//example.com/3")
 (:redirect-permanent . "//example.com/2")
 (:redirect-permanent . "//example.com/1")
 (:redirect-permanent . "//example.com"))
Goodbye.
;; As a footnote, gmi-too-many-requests is a child type of gmi-error.


;; You can configure your proxy:
* (gemini-request "//example.com" :proxy "proxy.example.com")
* (gemini-request "//example.com"
                  :proxy '("proxy-example.com" 8000))
* (let ((*gemini-default-proxy* "proxy-example.com"))
    (gemini-request "//example.com"))


;; You can choose whether to verify SSL certificates (for example,
;; expressly allowing self-signed certificates, or forcing servers
;; to be signed by a trusted root authority). Recommended reading
CL+SSL's MAKE-SSL-CLIENT-STREAM for all valid keyword arguments except
for EXTERNAL-FORMAT:
* (gemini-request "//self-signed.example.com" :ssl-options '(:verify nil))

;; You can use gemini-request-stream to return an input stream as the
;; primary value instead of the body. For text/* mimetype responses,
;; the stream is a flexi-stream with the automatically configured
;; external format, and for other responses, it's a raw octet stream.
* (gemini-request-stream "//example.com")
#<FLEXI-STREAMS:FLEXI-IO-STREAM {1234567890}>
20
"application/json; charset=utf-8; lang=en"
"application/json"
(("charset" . "utf-8") ("lang" . "en"))
#<PURI:URI //example.com/>

;; Finally, you can use gemini-request-stream* to
;; send a singular request and don't follow redirects.
* (gemini-request-stream* "//host.example.com/info.json"
                   "host.example.com" +gemini-default-port+
                   '(:verify nil))
#<FLEXI-STREAMS:FLEXI-IO-STREAM {1234567890}>
20
"application/json"
"application/json"
()
```
