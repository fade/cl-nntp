(in-package #:com.liotev.nntp)

(defparameter *default-client* nil
  "This client will be used in all functions that accept a client as an
optional parameter, and this optional parameter is not supplied or
nil.")

(defparameter *cache-authinfo-p* nil
  "If t, the user name and password will be saved in the client
object.")

(defparameter *authinfo-file-name*
  (str (user-homedir-pathname)
       ".authinfo")
  "The authinfo file name.")

(defparameter *reconnect-on-timeout* t
  "Controls if reconnecting is attempted after receiving timeout message
form the server.")

(defparameter *reconnect-on-error* t
  "Controls if reconnecting is attempted after an error.")

(defparameter *use-tls* nil
  "Controls if TLS is used for connecting to the server.")

(defparameter *authinfo-config-function*
  (list 'read-authinfo-file 'ask-autinfo-from-user)
  "Function used to read the authinfo configuration.")

(defparameter *clients* nil
  "A list to hold all clients.")

(defstruct client
  "An nntp client."
  host port last-command status-code status-message
  stream greating user pass group
  article-id article-number tls-p)

(define-condition client-timeout (error)
  ((client :initarg :client
           :reader client))
  (:documentation
   "A condition to be signaled when the NNTP server sends
a timeout message.")
  (:report
   (lambda (condition stream)
     (let ((user (client-user (client condition)))
           (host (client-host (client condition))))
       (format stream
               "Client ~A got timeout message from server ~A~%"
               (str user "@" host) host)))))

(defun open-stream-to-server
    (host port &key (use-tls nil use-tls-supplied?))
  (let ((stream (usocket:socket-stream
                 (usocket:socket-connect host port))))
    (if use-tls-supplied?
        (if use-tls (cl+ssl:make-ssl-client-stream stream)
            (values stream nil))
        (if (or (string= (str port) "563")
                (string= (str port) "443")
                *use-tls*)
            (values (cl+ssl:make-ssl-client-stream stream) t)
            (values stream nil)))))

(defun connect (host port &optional client
                &key auth (use-tls nil use-tls-supplied?))
  "Connects to the host. If a client is supplied just reconnects the
client."
  (format t "-> Connecting to ~a:~a~%" host port)
  (multiple-value-bind (stream tls-p)
      (if use-tls-supplied?
          (open-stream-to-server host port :use-tls use-tls)
          (open-stream-to-server host port))
    (let ((server-greeting (if tls-p (read-line-tls stream)
                               (read-line stream))))
      (format t "<- ~a~%" server-greeting)
      (force-output)
      (if (check-status-code (subseq server-greeting 0 3) '("200" "201"))
          (if (null client)
              (let ((new-client
                     (make-new-client host port stream server-greeting
                                      tls-p)))
                (register-client new-client)
                (setf *default-client* new-client)
                (when auth (authenticate-client new-client)))
              (progn
                (reset-client client host port stream server-greeting
                              tls-p)
                (when auth (authenticate-client client))))
          (error "Invalid server response: ~a~%" server-greeting)))))

(defun authenticate-client (client)
  (dolist (func *authinfo-config-function*)
    (let ((user-and-pass (funcall func (client-host client))))
      (when user-and-pass
        (apply #'authinfo
               (append user-and-pass
                       (list client)))
        (return client)))))

(defun make-new-client (host port stream server-greeting tls-p)
  (make-client
   :host host
   :port port
   :stream stream
   :status-message server-greeting
   :greating server-greeting
   :status-code (subseq server-greeting 0 3)
   :tls-p tls-p))

(defun reset-client (client host port stream server-greeting tls-p)
  (setf (client-host client) host)
  (setf (client-port client) port)
  (setf (client-stream client) stream)
  (setf (client-greating client) server-greeting)
  (setf (client-status-code client) (subseq server-greeting 0 3))
  (setf (client-tls-p client) tls-p)
  client)


(defun do-command
    (command &optional (client *default-client*)
     &key (reconnect t reconnect-supplied-p))
  "Sends the command to the server. In case of a timeout message from
the server, a reconnect will be attempted in the following cases:

   * the 'reconnect' key is supplied and it is t

   * the 'reconnect' key is not supplied and *reconnect-on-timeout* is t

In case of another error, a reconnect will be attempted if
*reconnect-on-error* is t.
"
  (if (or (and reconnect-supplied-p reconnect)
          (and (not reconnect-supplied-p) *reconnect-on-timeout*))
      (handler-bind ((client-timeout #'reconnect)
                     (error #'(lambda (c)
                                (when *reconnect-on-error*
                                  (reconnect c)))))
        (send-command command client))
      (send-command command client)))

(defun reconnect (condition)
  "Function for the 'rconnect' restart"
  (format t "A condition of type ~a was signaled: ~A~%"
          (type-of condition) condition)
  (force-output)
  (when (find-restart 'reconnect)
    (invoke-restart 'reconnect)) )

(defun send-command (command  &optional (client *default-client*))
  "Sends the command to the server."
  (setf (client-last-command client) command)
  (restart-case
      (progn (client-write-line command client)
             (when (string= (get-status client) "480")
               (authenticate-client client)
               (client-write-line command client)
               (get-status client))
             (when (timeout? client)
               (error 'client-timeout :client client))
             (values (client-status-code client)
                     (client-status-message client)))
    (reconnect ()
      (format t "Reconnecting to server ~A~%" (client-host client))
      (force-output)
      (reconnect-client client)
      (let ((*reconnect-on-error* nil))
        (send-command command client)))))

(defun timeout? (client)
  (when (and (string= "503" (client-status-code client))
             (or (search "time out" (client-status-message client))
                 (search "timeout" (client-status-message client))))
    t))


(defun group (group-name &optional (client *default-client*))
  "The GROUP nntp command.
Syntax
  GROUP group

Responses
  211 number low high group  Group successfully selected
  411                        No such newsgroup

Parameters
  group     Name of newsgroup
  number    Estimated number of articles in the group
  low       Reported low water mark
  high      Reported high water mark
"
  (setf (client-group client) group-name)
  (do-command (str "group " group-name) client))

(defun article (&optional (client *default-client*)
                &key article-number article-id)
  "Retreives an article.
Syntax
  ARTICLE message-id
  ARTICLE number
  ARTICLE

Responses

First form (message-id specified)
  220 0|n message-id    Article follows (multi-line)
  430                   No article with that message-id

Second form (article number specified)
  220 n message-id      Article follows (multi-line)
  412                   No newsgroup selected
  423                   No article with that number

Third form (current article number used)
  220 n message-id      Article follows (multi-line)
  412                   No newsgroup selected
  420                   Current article number is invalid

Parameters
  number        Requested article number
  n             Returned article number
  message-id    Article message-id
"
  (article-command "article" client :article-number article-number
                   :article-id article-id)
  (when (string= "220" (client-status-code client))
    (get-block-response client)))

(defun head (&optional (client *default-client*)
             &key article-number article-id)
  "Retreives the article headers.
 Syntax
   HEAD message-id
   HEAD number
   HEAD

 Responses

 First form (message-id specified)
   221 0|n message-id    Headers follow (multi-line)
   430                   No article with that message-id

 Second form (article number specified)
   221 n message-id      Headers follow (multi-line)
   412                   No newsgroup selected
   423                   No article with that number

 Third form (current article number used)
   221 n message-id      Headers follow (multi-line)
   412                   No newsgroup selected
   420                   Current article number is invalid
"
  (article-command "head" client :article-number article-number
                   :article-id article-id)
  (when (string= "221" (client-status-code client))
    (get-block-response client)))

(defun body (&optional (client *default-client*)
             &key article-number article-id)
  "Retreives the article body
Syntax
  BODY message-id
  BODY number
  BODY

Responses

First form (message-id specified)
  222 0|n message-id    Body follows (multi-line)
  430                   No article with that message-id

Second form (article number specified)
  222 n message-id      Body follows (multi-line)
  412                   No newsgroup selected
  423                   No article with that number

Third form (current article number used)
  222 n message-id      Body follows (multi-line)
  412                   No newsgroup selected
  420                   Current article number is invalid

Parameters
  number        Requested article number
  n             Returned article number
  message-id    Article message-id
"
  (article-command "body" client :article-number article-number
                   :article-id article-id)
  (when (string= "222" (client-status-code client))
    (get-block-response client)))

(defun stat (&optional(client *default-client*)
             &key article-number article-id)
  "Determines if an article exists, or the message
id of the article.

Syntax
  STAT message-id
  STAT number
  STAT

Responses

First form (message-id specified)
  223 0|n message-id    Article exists
  430                   No article with that message-id

Second form (article number specified)
  223 n message-id      Article exists
  412                   No newsgroup selected
  423                   No article with that number

Third form (current article number used)
  223 n message-id      Article exists
  412                   No newsgroup selected
  420                   Current article number is invalid

Parameters
  number        Requested article number
  n             Returned article number
  message-id    Article message-id
"
  (article-command "stat" client :article-number article-number
                   :article-id article-id))

(defun capabilities (&optional (client *default-client*))
  "Lists the capabilities of the server.
Syntax
  CAPABILITIES [keyword]

Responses
  101    Capability list follows (multi-line)
"
  (block-command "CAPABILITIES" (list "101") client))

(defun help (&optional (client *default-client*))
  (block-command "help" (list "100") client))

(defun block-command (command valid-codes
                      &optional (client *default-client*))
  "Sends a command that expects a block and retreives the block
response."
  (do-command command client)
  (when (find (client-status-code client) valid-codes :test #'string=)
    (get-block-response client)))

(defun article-command (command &optional (client *default-client*)
                        &key article-number article-id)
  "Performs an articla related command. Can be one of 'ARTICLE', 'HEAD',
'BODY', 'STAT'."
  (if (and (null article-number)
           (null article-id))
      (do-command command client)
      (let ((full-command
             (string-trim " " (str command " "
                                   (if (null article-number)
                                       article-id
                                       article-number)))))
        (set-article client :article-number article-number
                     :article-id article-id)
        (do-command full-command client))))

(defun set-article (&optional(client *default-client*)
                    &key article-number article-id)
  "Sets the article in the client"
  (setf (client-article-number client) article-number
        (client-article-id client) article-id))

(defun get-status (&optional (client *default-client*))
  "Reads the status line of the server response, retrurn status code and
status message."
  (let ((line (client-read-line client)))
    (if line
        (progn
          (format t "<- ~A~%" line)
          (force-output)
          (let ((status-code (subseq line 0 (position #\space line)))
                (status-message (subseq line (1+ (position #\space line)))))
            (setf (client-status-message client) status-message
                  (client-status-code client) status-code)
            (values status-code status-message)))
        (error "No response from the server"))))

(defun authinfo (user pass &optional (client *default-client*))
  "Authenticates the client."
  (when (null client) (error "Can not authenticate NIL client"))
  (when *cache-authinfo-p*
    (setf (client-user client) user)
    (setf (client-pass client) pass))
  (authinfo-user user client)
  (cond ((string= (client-status-code client) "381")
         (authinfo-pass pass client)
         (cond ((string= (client-status-code client) "281")
                client)
               (t (error "Couldnt authenticate client: ~a"
                         (client-status-message client)))))
        (t (error "Couldnt authenticate client: ~a"
                  (client-status-message client)))))

(defun register-client (client)
  (format t "Registering client ~a ~%" (client-name client))
  (when (null (find client *clients* :test #'eq))
    (format t "Adding client to *clients*~%")
    (setf *clients* (cons client *clients*))))

(defun client-name (client)
  (let ((name (str (client-host client) ":" (client-port client)))
        (user (client-user client)))
    (if user (str user "@" name) name)))

(defun authinfo-user (user &optional (client *default-client*))
  "Sends the 'authinfo user' command."
  (let ((command (str "AUTHINFO USER " user)))
    (client-write-line command client)
    (get-status client)))

(defun authinfo-pass (pass &optional (client *default-client*))
  "Sends the 'authinfo pass' command."
  (let ((command (str "AUTHINFO PASS " pass)))
    (client-write-line command client)
    (get-status client)))

(defun date (&optional (client *default-client*))
  (do-command "date" client))

(defun last-article (&optional (client *default-client*))
  (do-command "last" client))

(defun next-article (&optional (client *default-client*))
  (do-command "next" client))

(defun get-block-response (&optional (client *default-client*))
  "Reads a block response."
  (let ((stream (client-stream client)))
    (with-output-to-string (s)
      (loop
         (multiple-value-bind (line nl)
             (client-read-line client nil stream)
           ;; (format t "<- ~A~%" line)
           (when (or (string= line ".")
                     (string= line ".")
                     (eq line stream))
             (return s))
           (write-string line s)
           (unless nl
             (write-char #\Newline s)))))))

(defun check-status-code (status-code valid-codes)
  (if (find status-code valid-codes :test #'string=)
      status-code
      nil))

(defun disconnect-client (&optional (client *default-client*))
  (let ((stream (client-stream client)))
    (when (or (null stream)
              (not (open-stream-p stream)))
      (format t "Client ~a:~a is not connected.~%"
              (client-host client) (client-port client))
      (return-from disconnect-client client))
    (close stream)
    (if (open-stream-p stream)
        (error "Couldn't close stream: ~A~%" stream)
        (setf (client-stream client) nil))
    client))

(defun destroy-client (&optional (client *default-client*))
  (disconnect-client client)
  (setf *clients* (delete client *clients*))
  (when (eq *default-client* client)
    (setf *default-client* (car *clients*))))

(defun reconnect-client (&optional (client *default-client*))
  (connect (client-host client) (client-port client) client)
  (when (client-group client)
    (let ((command (str "group " (client-group client))))
      (client-write-line command client)
      (get-status client)))
  (cond ((client-article-number client)
         (client-write-line
          (str "stat " (client-article-number client))
          (get-status client)))
        ((client-article-id client)
         (client-write-line (str "stat " (client-article-id client)) client)
         (get-status client))
        (t nil)))

(defun read-authinfo-file (server-name &optional
                           (file-name *authinfo-file-name*))
  "Reads user name and passwords from authinfo file."
  (when (not (probe-file file-name))
    (return-from read-authinfo-file nil))
  (let ((lines (split-sequence #\newline (slurp-file file-name))))
    (loop for line in lines
       do (let ((tokens (split-sequence #\space line)))
            (when (and (string= server-name (cadr tokens))
                       (>= (length tokens) 6))
              (return-from read-authinfo-file
                (list (fourth tokens) (sixth tokens))))))))

(defun read-line-tls (tls-stream &optional (eof-error-p t) (eof-value nil)
                      &key max-length)
  "Reads a line from a TLS (SSL) stream."
  (with-output-to-string (s)
    (do ((b (read-byte tls-stream) (read-byte tls-stream))
         (count 0 (1+ count)))
        ((eq (code-char b) #\newline))
      (when (eq b 'eof)
        (if (and eof-error-p (= 0 (length s)))
            (return-from read-line-tls (values eof-value t))
            (return-from read-line-tls (values s t))))
      (write-char (code-char b) s)
      (when (and max-length (> count max-length))
        (error "Line to long: ~a~%." s)))
    s))

(defun write-line-tls (line tls-stream )
  "Writes a line to  a TLS (SSL) stream."
  (let ((new-line (if (position #\newline line
                                :start (1- (length line)))
                      line
                      (str line #\newline))))
    (format t "-> ~a" new-line)
    (write-sequence (babel:string-to-octets new-line) 
                    tls-stream)))

(defun client-read-line (client &optional (eof-error-p t) (eof-value nil))
  (if (client-tls-p client)
      (read-line-tls (client-stream client))
      (read-line (client-stream client)
                 eof-error-p eof-value)))

(defun client-write-line (line client)
  (if (client-tls-p client)
      (write-line-tls line (client-stream client))
      (progn
        (format t "-> ~a~%" line)
        (write-line (str line #\newline) (client-stream client))))
  (force-output (client-stream client)))

(defun ask-autinfo-from-user (server-name)
  (format t "Please enter user name for server ~a~%"
          server-name)
  (let ((user-name (read-line)))
    (format t "Please enter password for server ~a~%"
            server-name)
    (let ((passw (read-line)))
      (list user-name passw))))
