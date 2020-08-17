# cl-gemini

A library for communication to servers through the Gemini protocol.

---

Project Gemini is a semi-recent internet protocol protject that
describes itself as somewhere between Gopher and the convnetional
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

Drop this project in a place that ADSF can see it - I personally
develop this library in `~/quicklisp/local-projects/` so that you can
use quicklisp to pull in its dependencies, `alexandria`, `cl+ssl`,
`puri`, and `trivial-sockets`.

```lisp
* (require :cl-gemini)
* (use-package :cl-gemini)

;; Make a simple gemini request. It returns the response body,
;; code, meta text (mimetype for successful requests), and the url of
;; the last request (say, if gemini-request handles redirects):
* (gemini-request "gemini://gemini.circumlunar.space")
"# Project Gemini

## Overview

Gemini is a new internet protoc...ol which:

* Is heavier than gopher
* Is lighter than th..."
20
"text/gemini"
"gemini://gemini.circumlunar.space/"
```

## Extended Usage

```lisp
;; Receiving an error code sends a gmi-error by default.
* (handler-case
      (gemini-request "gemini://example.com/no-exist")
    (gmi-error (err) (format t "Received response ~D.~%" 
                             (gmi-code err))))
Received response 51.
;; An error is thrown for any valid 4x, 5x, and 6x response codes, as
;; well as any unrecognized response codes. 1x and 2x response codes
;; are returned as normal, and 3x redirect response codes are handled
;; automatically.
      

;; You can disable this by setting gemini-error-p to NIL:
* (gemini-reqeust "gemini://example.com/no-exist"
                  :gemini-error-p nil)
NIL
51
"Not found!"
"gemini://example.com/no-exist"

;; You can configure the maximum redirects to automatically follow
(default is 5).
* (handler-case
      (gemini-request "gemini://example.com" :max-redirects 3)
    (gmi-too-many-requests (cond)
      (format t "Too many requests!~%~S~%Goodbye.~%"
              (gmi-redirect-trace cond)))
Too many requests!
((:redirect-permanent . "gemini://example.com/3")
 (:redirect-permanent . "gemini://example.com/2")
 (:redirect-permanent . "gemini://example.com/1")
 (:redirect-permanent . "gmeini://example.com"))
Goodbye.

;; You can configure your proxy:
* (gemini-request "gemini://example.com" :proxy "proxy.example.com")
* (gemini-request "gemini://example.com" 
                  :proxy '("proxy-example.com" 8000))
* (let ((*gemini-default-proxy* "proxy-example.com"))
    (gemini-request "gemini://example.com"))

;; You can choose whether to verify SSL certificates (for example,
;; expressly allowing self-signed certificates or forcing servers
;; to be signed by a trusted root authority)
* (gemini-request "gemini://self-signed.example.com" :verify-ssl nil)

* (let ((*gemini-default-verify-ssl* nil))
    (gemini-request "gemini://self-signed.example.com" 
                    :verify-ssl nil))
                    
;; For more details:
(describe '*gemini-default-verify-ssl*)

;; Finally, for lower-level control, You can use gemini-request* to
;; send a singular request and don't do anything smart with it. It
;; only returns three values, since it doesn't resolve redirects.
* (gemini-request "gemini://host.example.com/link-that-sends-code-42"
                  "host.example.com" +gemini-default-port+
                  *gemini-default-verify-ssl*)
NIL
42
"CGI Error"
```
