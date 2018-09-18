;;; Utility library for loading .env files
;;; Author: Olle Lauri Boström <ollebostr@gmail.com>
;;; Documentation: https://github.com/ollelauribostrom/cl-dotenv
;;; Inspiration from http://cl-cookbook.sourceforge.net/os.html & http://www.lispforum.com/viewtopic.php?f=2&t=446 & https://www.npmjs.com/package/dotenv

(defpackage #:cl-dotenv
  (:use #:cl
        #:alexandria
        #:serapeum)
  (:export #:load-env
           #:read-env
           #:malformed-entry
           #:duplicated-entry)
  (:nicknames #:dotenv
              #:.env)
  (:documentation "
.ENV files are parsed according to the following rules.

  ∙ Empty lines are skipped.
  ∙ Lines beginning with # are treated as comments
  ∙ Empty values are treated as empty strings, e.j. 'EMPTY=' sets the
    environment variable EMPTY to ''."))

(in-package #:cl-dotenv)

(define-condition malformed-entry (error)
  ((line :initarg :line :reader line))
  (:report
   (lambda (condition stream)
     (format stream "Malformed entry: ~A." (line condition)))))

(define-condition duplicated-entry (error)
  ((key :initarg :key :reader key)
   (prev-value :initarg :prev-value :reader prev-value)
   (value :initarg :value :reader value))
  (:report
   (lambda (condition stream)
     (format stream "Duplicated entry: ~A. ~A would overwrite ~A~%"
             (key condition) (value condition) (prev-value condition)))))

(defun whitespace-p (char)
  (member char '(#\Space #\Tab #\Vt #\Page #\NO-BREAK_SPACE)))

(defun read-entry (line)
  (cond ((zerop (length line))
         (values nil nil nil))
        ((char= #\# (aref line 0))
         (values nil nil nil))
        ((every #'whitespace-p line)
         (values nil nil nil))
        (t
         (if-let (=-pos (position #\= line))
           (let ((name (subseq line 0 =-pos))
                 (value (subseq line (1+ =-pos))))
             (values name value t))
           (error 'malformed-entry :line line)))))

(defun read-env (path)
  (lret ((env (make-hash-table :test 'equalp)))
    (with-open-file (in path)
      (loop
        :for line := (read-line in nil)
        :while line
        :do (multiple-value-bind
                  (key value foundp)
                (restart-case
                    (read-entry line)
                  (skip-entry () nil))
              (when foundp
                (setf (gethash key env)
                      (restart-case
                          (if-let (prev-value (gethash key env))
                            (error 'duplicated-entry :key key
                                                     :prev-value prev-value
                                                     :value value)
                            value)
                        (skip-entry () (gethash key env))
                        (overwrite-entry () value)))))))))

(defun %load-env (env)
  (maphash (lambda (name value)
             (setf (uiop:getenv name) value))
           env))

(defun load-env (path)
  (%load-env (read-env path)))
