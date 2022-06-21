(load "package.lisp")
(load "system.lisp")
(load "main.lisp")

(sb-ext:save-lisp-and-die "forge-bin"
                          :toplevel 'main:main
                          :executable t)
