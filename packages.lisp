
(defpackage #:com.liotev.nntp.utils
  (:use #:cl)
  (:export #:str #:slurp-file))

(defpackage #:com.liotev.nntp
  (:nicknames #:cl-nntp #:nntp)
  (:use #:cl #:split-sequence #:com.liotev.nntp.utils)
  (:export #:connect
           #:group
           #:article
           #:head
           #:body
           #:stat
           #:last-article
           #:next-article
           #:authinfo
           #:help
           #:capabilities
           #:date
           #:quit
           ;; #:listgroup
           ;; #:mode-reader
           ;; #:list
           ;; #:newsgroups
           ;; #:newnews
           ;; #:post
           ;; #:xhdr
           ;; #:xover
           ;; #:xpat
           ))

