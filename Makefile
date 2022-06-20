help:
	@echo "Use a command!"

deps:
	@echo "Making sure SBCL is installed..."
ifneq ($(shell command -v xbps-query),)
	sudo xbps-install -Syu sbcl
else ifneq ($(shell command -v pacman),)
    sudo pacman -Sy sbcl
else ifneq ($(shell command -v dnf),)
    sudo dnf install -y sbcl
else ifneq ($(shell command -v apt),)
    sudo apt install -y sbcl
else ifneq ($(shell command -v sbcl),)
	@echo "SBCL found!"
else
	@echo "Could not determine steps to install SBCL! Please install SBCL and try again."
endif

req: deps

quicklisp:
	curl https://beta.quicklisp.org/quicklisp.lisp -o /tmp/quicklisp.lisp
	sbcl --script ./bootstrap-quicklisp.lisp

install: req quicklisp
	@echo "forge is now installed."
