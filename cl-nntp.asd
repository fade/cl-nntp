;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package #:cl-user)

(defpackage :cl-nntp-system
  (:use #:asdf #:cl))

(in-package #:cl-nntp-system)

(defsystem :cl-nntp
  :author "Dimitre Liotev <dl@liotev.com>"
  :version "1.0"
  :maintainer "Dimitre Liotev <dl@liotev.com>"
  :licence "GPL"
  :description "An NNTP client library"
  :components ((:file "packages")
               (:file "utils" :depends-on ("packages"))
               (:file "nntp" :depends-on ("packages" "utils")))
  :depends-on (#:usocket #:split-sequence #:cl+ssl #:babel))
