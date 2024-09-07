
(load "lisp-maze.asd")
(asdf:load-system :lisp-maze)
(ext:saveinitmem #p"./build/foo-clisp" :init-function 
                 (lambda () (init-fun) (ext:quit))
                 :executable t :quiet t :norc t)
