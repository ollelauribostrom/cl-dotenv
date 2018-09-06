;;; Utility library for loading .env files
;;; Author: Olle Lauri Boström <ollebostr@gmail.com>
;;; Documentation: https://github.com/ollelauribostrom/cl-dotenv
;;; Inspiration from http://cl-cookbook.sourceforge.net/os.html & http://www.lispforum.com/viewtopic.php?f=2&t=446 & https://www.npmjs.com/package/dotenv

(defpackage cl-dotenv
  (:use :cl :cl-ppcre)
  (:export :load-env :get-env :set-env)
  (:nicknames :dotenv))
(in-package :cl-dotenv)

(defun load-env(path)
  (let ((env (uiop:read-file-lines path)))
    (loop for line in env
      do (destructuring-bind (&optional name value &rest _) (cl-ppcre:split "=" line)
        (if (and name value)
          (set-env name value)
          (error "Invalid .env file"))))
    t))

(defun get-env (name &optional default)
  (or
    #+Allegro (sys:getenv name)
    #+CLISP (ext:getenv name)
    #+ECL (si:getenv name)
    #+SBCL (sb-unix::posix-getenv name)
    #+LISPWORKS (lispworks:environment-variable name)
    default))

(defun set-env(name value)
  (or
    #+Allegro (setf (sys:getenv name) value)
    #+CLISP (setf (ext:getenv name) value)
    #+ECL (si:setenv name value)
    #+SBCL (sb-posix:setenv name value 1)
    #+LISPWORKS (setf (lispworks:environment-variable name) value)
    (error "set-env is not supported for your Lisp implementation"))
  t)