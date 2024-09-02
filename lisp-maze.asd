
(asdf:defsystem :lisp-maze
  :components ((:file "main" :depends-on ("ncurses-call"))
               (:file "ncurses-call"))
  :depends-on (:cffi))
