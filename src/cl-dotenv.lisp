;;; Utility library for loading .env files
;;; Author: Olle Lauri Boström <ollebostr@gmail.com>
;;; Documentation: https://github.com/ollelauribostrom/cl-dotenv
;;; Inspiration from http://cl-cookbook.sourceforge.net/os.html & http://www.lispforum.com/viewtopic.php?f=2&t=446 & https://www.npmjs.com/package/dotenv

(defpackage cl-dotenv
  (:use :cl :cl-ppcre :uiop)
  (:export :load-env :get-env :set-env)
  (:nicknames :dotenv))
(in-package :cl-dotenv)

(defun load-env(path)
  (let ((env (uiop:read-file-lines path)))
    (loop for line in env
      do (destructuring-bind (&optional name value &rest _) (cl-ppcre:split "=" line)
        (if (and name value)
          (set-env name value)
          (error "Invalid .env file"))))))

(defun get-env (name &optional default)
  (or
    (uiop:getenv name)
    default))

(defun set-env(name value)
  (setf (uiop:getenv name)
        value))
