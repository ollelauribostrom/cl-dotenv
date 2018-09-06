.PHONY: install test coverage sbcl

install:
	ros install ./cl-dotenv.asd && \
	ros install ./cl-dotenv-test.asd

test:
	run-prove cl-dotenv-test.asd

coverage:
	sbcl --non-interactive\
		--eval "(pushnew *default-pathname-defaults* asdf:*central-registry*)" \
		--eval "(ql:register-local-projects)" \
		--eval "(ql:quickload :prove)" \
		--eval "(require :sb-cover)" \
		--eval "(declaim (optimize sb-cover:store-coverage-data))" \
		--eval "(asdf:oos 'asdf:load-op :cl-dotenv :force t)" \
		--eval "(prove:run :cl-dotenv-test)" \
		--eval "(sb-cover:report \"coverage/\")" \
		--eval "(declaim (optimize (sb-cover:store-coverage-data 0)))" && \
		open coverage/cover-index.html

sbcl:
	sbcl \
		--eval "(pushnew *default-pathname-defaults* asdf:*central-registry*)" \
		--eval "(ql:register-local-projects)" \
		--eval "(ql:quickload :cl-dotenv)"
