(defsystem "cl-dotenv"
  :version "0.1.0"
  :author "Olle Lauri Boström"
  :license "MIT"
  :depends-on ("alexandria"
               "serapeum")
  :pathname "src"
  :components ((:file "cl-dotenv"))
  :description "Utility library for loading .env files"
  :in-order-to ((test-op (test-op "cl-dotenv-test"))))
