;#!/home/vboxuser/.roswell/impls/x86-64/linux/sbcl-bin/2.3.2/bin/sbcl --script

(load "lisp-maze.asd")
(asdf:load-system :lisp-maze)
(sb-ext:save-lisp-and-die #p"./build/foo-sbcl" 
                          :toplevel
                          #'init-fun
                          :executable t)
