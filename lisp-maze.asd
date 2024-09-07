
(asdf:defsystem
 :lisp-maze
 :components ((:file "main" :depends-on ("ncurses-call" "maze-generator"))
              (:file "ncurses-call")
              (:file "maze-generator"))
 :depends-on (:cffi))
