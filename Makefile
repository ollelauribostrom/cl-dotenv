.PHONY: load test

load:
	sbcl --eval "(pushnew *default-pathname-defaults* asdf:*central-registry*)" --eval "(ql:register-local-projects)" --eval "(ql:quickload :cl-dotenv)"

test:
	sbcl --eval "(pushnew *default-pathname-defaults* asdf:*central-registry*)" --eval "(ql:register-local-projects)" --eval "(asdf:test-system :cl-dotenv)" --non-interactive