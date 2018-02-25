(defsystem "cl-dotenv-test"
  :defsystem-depends-on ("prove-asdf")
  :author "Olle Lauri Boström"
  :license "MIT"
  :depends-on ("cl-dotenv"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "cl-dotenv"))))
  :description "Test system for cl-dotenv"

  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
