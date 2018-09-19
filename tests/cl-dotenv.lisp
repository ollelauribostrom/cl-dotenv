(defpackage #:cl-dotenv/tests
  (:use #:cl
        #:cl-dotenv
        #:prove))
(in-package #:cl-dotenv/tests)

(plan 1)

(subtest "cl-dotenv:read-env"
  (subtest "READ-ENV throws error if .env file contains malformed entries"
    (is-error (.env:read-env (asdf:system-relative-pathname "cl-dotenv-test"
                                                            "./tests/.env-malformed"))
              'malformed-entry))

  (subtest "READ-ENV throws error if .env file contains duplicated entries"
    (is-error (.env:read-env (asdf:system-relative-pathname "cl-dotenv-test"
                                                            "./tests/.env-duplicated"))
              'duplicated-entry))

  (subtest "READ-ENV loads a .env file"
    (let ((env (.env:read-env (asdf:system-relative-pathname "cl-dotenv-test"
                                                             "./tests/.env-test"))))
      (is (gethash "TEST_VAR_1" env) "test1")
      (is (gethash "TEST_VAR_2" env) "test2")
      (is (gethash "TEST_VAR_3" env) "")))

  (subtest "LOAD-ENV  sets the environment"
    (.env:load-env (asdf:system-relative-pathname "cl-dotenv-test"
                                                  "./tests/.env-test"))
    (is (uiop:getenv "TEST_VAR_1") "test1")
    (is (uiop:getenv "TEST_VAR_2") "test2")
    (is (uiop:getenv "TEST_VAR_3") ""))

  (subtest "CONDITION: malformed-entry is formatted correctly" 
    (is-print (handler-case (error 'malformed-entry :line "MALFORMED")
                (error (c)
                  (format t "~a" c))) 
              "Malformed entry: MALFORMED."))

  (subtest "CONDITION: duplicated-entry is formatted correctly" 
    (is-print (handler-case (error 'duplicated-entry :key "KEY"
                                                     :prev-value "FIRST"
                                                     :value "SECOND")
                (error (c)
                  (format t "~a" c))) 
              "Duplicated entry: KEY. SECOND would overwrite FIRST")))

(finalize)
