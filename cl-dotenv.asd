(defsystem "cl-dotenv"
  :version "0.1.0"
  :author "Olle Lauri Boström"
  :license "MIT"
  :depends-on ("cl-ppcre"
               "uiop")
  :components ((:module "src"
                :components
                ((:file "cl-dotenv"))))
  :description "Utility library for loading .env files"
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op "cl-dotenv-test"))))
