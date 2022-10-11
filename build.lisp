(ql:quickload "trivial-raw-io")
(load "package.lisp")
(load "src/utils.lisp")
(load "src/shell.lisp")
(load "src/main.lisp")

(sb-ext:save-lisp-and-die "forge-bin"
                          :toplevel 'main:main
                          :executable t)
