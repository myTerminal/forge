help:
	@echo "Use a command!"

sbcl:
	@echo "Making sure SBCL is installed..."
ifneq ($(shell command -v sbcl),)
	@echo "SBCL found."
else
	@echo "SBCL not found!"
	@echo "Attempting to install SBCL using Crater..."
	git clone https://github.com/crater-space/cli /tmp/crater-cli
	/tmp/crater-cli/crater install sbcl
	rm -rf /tmp/crater-cli
endif

quicklisp:
	curl https://beta.quicklisp.org/quicklisp.lisp -o /tmp/quicklisp.lisp
	sbcl --load /tmp/quicklisp.lisp --non-interactive --eval "(quicklisp-quickstart:install)"
	sbcl --load ~/quicklisp/setup.lisp --non-interactive --eval "(ql:add-to-init-file)"

binary:
	@echo "Generating binary"
	sbcl --non-interactive --load build.lisp

setup: sbcl quicklisp binary
	@echo "forge setup complete."
