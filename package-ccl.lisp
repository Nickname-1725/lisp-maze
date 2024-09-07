
(load "lisp-maze.asd")
(asdf:load-system :lisp-maze)
(ccl:save-application #p"./build/foo-ccl" :toplevel-function #'init-fun
                      :prepend-kernel t)

