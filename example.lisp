
;; This an example of fetching an NNTP article. The server nntp.aioe.org
;; is free and does not require any registration, so you should have no
;; problems connecting to it. If you do get a connection error try
;; another nntp server.

;; To load with SBCL:
;; sbcl --load example.lisp

;; To load with ClozureCL (CCL):
;; ccl --load example.lisp

(asdf:load-system "cl-nntp")

(cl-nntp:connect "nntp.aioe.org" 119)
(cl-nntp:group "comp.lang.lisp")
(cl-nntp:next-article)
(let ((article (cl-nntp:article)))
  ;; write the article to standard output
  (write article))
