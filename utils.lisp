
(in-package #:com.liotev.nntp.utils)

(defun str (&rest parts)
  (with-output-to-string (out)
    (dolist (part parts)
      (when (not (null part))
        (write part :stream out :escape nil)))))

(defun slurp-stream-as-seq (stream &key (element-type 'base-char))
  (let ((seq (make-array (file-length stream) :element-type element-type :fill-pointer t)))
    (setf (fill-pointer seq) (read-sequence seq stream))
    seq))

(defun slurp-stream-in-chunks (stream &key (element-type 'character) (chunk-length 1024))
  (with-output-to-string (out)
    (let ((seq (make-array chunk-length :element-type element-type
                           :adjustable t
                           :fill-pointer chunk-length)))
      (loop
         (setf (fill-pointer seq) (read-sequence seq stream))
         (when (zerop (fill-pointer seq))
           (return))
         (write-sequence seq out)))))

(defun slurp-stream (stream &key (element-type 'base-char))
  (if (null (file-length stream))
      (slurp-stream-in-chunks stream)
      (slurp-stream-as-seq stream :element-type element-type)))

(defun slurp-file (file-name &key (element-type 'base-char))
  (with-open-file (s file-name :direction :input :element-type element-type)
    (slurp-stream-as-seq s :element-type element-type)))

(defun getenv (name &optional default)
  #+CMU
  (let ((x (assoc name ext:*environment-list*
                  :test #'string=)))
    (if x (cdr x) default))
  #-CMU
  (or
   #+Allegro (sys:getenv name)
   #+CLISP (ext:getenv name)
   #+ECL (si:getenv name)
   #+SBCL (sb-unix::posix-getenv name)
   #+LISPWORKS (lispworks:environment-variable name)
   #+CCL (getenv name)
   default))


