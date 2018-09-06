(defpackage cl-dotenv-test
  (:use :cl
        :cl-dotenv
        :prove))
(in-package :cl-dotenv-test)

(plan 3)

(subtest "cl-dotenv:get-env"
  (subtest "get-env uses provided default value if no env variable is found"
    (is (dotenv:get-env "NOT_SET" "default") "default"))
  (subtest "get-env returns nil if not env variable is found & no default is provided"
    (is (dotenv:get-env "NOT_SET") nil)))

(subtest "cl-dotenv:set-env"
  (subtest "set-env sets the environment"
    (is (and 
          (dotenv:set-env "TEST_VAR" "test") 
          (dotenv:get-env "TEST_VAR"))
        "test")))

(subtest "cl-dotenv:load-env"
  (subtest "load-env throws error if .env file is corrupt"
    (is-error (dotenv:load-env (merge-pathnames "./tests/.env-corrupt")) 'simple-error))
  (subtest "load-env loads a .env file & sets the environment"
    (dotenv:load-env (merge-pathnames "./tests/.env-test"))
    (is (dotenv:get-env "TEST_VAR_1") "test1")
    (is (dotenv:get-env "TEST_VAR_2") "test2")))

(finalize)
