
;;;; REPL
(defparameter *last-dir* nil)
(defun restart-maze (height width)
  (setf *user-ij* '(0 0))
  (setf *H-maze* height)
  (setf *W-maze* width)
  (setf *table-maze* (generate-table))
  (init-maze `(,(ash *H-maze* -1) ,(ash *W-maze* -1))))
(defun user-handler-create ()
  (let ((usr-ij (copy-list *user-ij*)))
    #'(lambda (dir)
        (let ((try-usr (copy-list usr-ij)))
          (cond ((eq dir 'w) (incf (car try-usr) -1))
                ((eq dir 'a) (incf (cadr try-usr) -1))
                ((eq dir 's) (incf (car try-usr) 1))
                ((eq dir 'd) (incf (cadr try-usr) 1))
                (t nil))
          (if (and (i-j-legal-p try-usr)
                   (link-p usr-ij dir))
              (progn
                (setf usr-ij try-usr))
              nil)))))

(defun repl()
  (let* ((cmd-string (read-line))
         (cmd-list (mapcar #'read-from-string
                           (mapcar #'string
                                   (remove-if #'(lambda(char)
                                                  (eq char #\ ))
                                              (coerce cmd-string 'list))))))
    (cond ((and (eq (length cmd-list) 1) (eq (car cmd-list) 'q)) (quit))
          ((and (eq (length cmd-list) 1) (eq (car cmd-list) 'r))
           (flet ((resize-repl()
                    (format t "Enter 2 numbers to customize the size. ~%")
                    (let* ((size-pair (read-from-string
                                       (concatenate 'string "(" (read-line) ")")))
                           (h (car size-pair))
                           (w (cadr size-pair)))
                      (cond ((not (eq 2 (length size-pair)))
                             (print "Size share be 2 numbers: height and width. ")
                             (read-line)
                             (repl))
                            ((not (and (numberp h) (numberp w)))
                             (print "That's not numbers. ")
                             (read-line)
                             (repl))
                            ((or (< h 2) (< w 2))
                             (print "Don't be ridiculous. height and width share be at least 2. ")
                             (read-line)
                             (repl))
                            ((or (> h 50) (> w 50))
                             (print "Don't be rediculous. height and width share be at most 50. ")
                             (read-line)
                             (repl))
                            (t (restart-maze h w)
                               (repl))))))
             (resize-repl)))
          (t (repl)))))

(defun init-gameboard (H-maze W-maze)
  (let* ((side-bar-width 25)
         (playground-size-y H-maze)
         (playground-size-x (1+ (* 2 W-maze)))
         (game-board-windows
           (ncurses-newwin
            playground-size-y (+ playground-size-x side-bar-width)
            (ash (- ncurses-lines playground-size-y) -1)
            (ash (- ncurses-cols playground-size-x side-bar-width) -1)))
         (playground-window
           (ncurses-derwin
            game-board-windows playground-size-y playground-size-x 0 0))
         (timerun-window
           (ncurses-derwin
            game-board-windows 1 side-bar-width 0 playground-size-x))
         (message-window
           (ncurses-derwin
            game-board-windows 2 side-bar-width (+ 1 0) playground-size-x))
         (input-window
           (ncurses-derwin
            game-board-windows 1 side-bar-width (+ 2 1 0) playground-size-x))
         (tips-window
           (ncurses-derwin
            game-board-windows (- playground-size-y 1 2 1)
            side-bar-width (+ 1 2 1 0) playground-size-x)))
    (ncurses-wbkgd playground-window (ncurses-color-pair playground-pair))
    (ncurses-wbkgd timerun-window (ncurses-color-pair menu-light-pair))
    (ncurses-wbkgd message-window (ncurses-color-pair menu-dark-pair))
    (ncurses-wbkgd input-window (ncurses-color-pair input-pair))
    (ncurses-wbkgd tips-window (ncurses-color-pair menu-light-pair))
    (values playground-window timerun-window message-window
            input-window tips-window)))

(defun dump-text-window (win text)
  (ncurses-wclear win)
  (ncurses-waddstr win text)
  (ncurses-wrefresh win))

(defun register-patch-to-window (win)
  (let (char-last y-last x-last)
    (lambda (y x str)
      (when char-last
        (ncurses-mvwaddstr win y-last x-last (string char-last)))
      (cffi:with-foreign-object (wchar '(:struct cchar-t))
        (ncurses-mvwin-wch win y x wchar)
        (setf char-last (code-char (cffi:foreign-slot-value
                                    wchar '(:struct cchar-t) 'chars)))
        (setf y-last y) (setf x-last x))
      (ncurses-mvwaddstr win y x str)
      (ncurses-wrefresh win))))

(defun enter-number (win &optional (number 0))
  (unless (eql 0 number)
    (dump-text-window win (format nil "~a" number)))
  (let* ((code (ncurses-getch))
         (ch (if (and (< code 256) (>= code 0))
                 (code-char code))))
    (cond
      ((case ch (#\space t) (#\newline t))
       (dump-text-window win "")
       number)
      ((and (<= code (char-code #\9)) (>= code (char-code #\0)))
       (enter-number win (+ (* 10 number) (- code (char-code #\0)))))
      (t(enter-number win number)))))

(defun init-fun ()
  (init-TUI)
  
  (let (playground-window timerun-window message-window input-window tips-window
        user-handler patch-maze h-maze w-maze)
    (labels ((game-on (h w)
               (ncurses-clear)
               (ncurses-refresh)
               ;; 准备上下文环境
               (when (or playground-window timerun-window message-window
                         input-window tips-window)
                 (ncurses-delwin playground-window)
                 (ncurses-delwin timerun-window)
                 (ncurses-delwin message-window)
                 (ncurses-delwin input-window)
                 (ncurses-delwin tips-window))
               (multiple-value-bind (ply-win tim-win msg-win in-win tps-win)
                   (init-gameboard h w)
                 (setf playground-window ply-win) (setf timerun-window tim-win)
                 (setf message-window msg-win) (setf input-window in-win)
                 (setf tips-window tps-win))
               (setf patch-maze (register-patch-to-window playground-window))
               (setf user-handler (user-handler-create))
               (setf h-maze h) (setf w-maze w)
               ;; 初始化界面信息
               (dump-text-window timerun-window "time: 00:00.00")
               (dump-text-window message-window "Press [Space] key to start.")
               (dump-text-window input-window ">_")
               (dump-text-window
                tips-window
                (format nil "[W] for up; ~%[A] for left; ~@
                             [S] for down; ~%[D] for right; ~@
                             [q] to leave the game. ~@
                             [r] to reshape the maze and restart the game. "))
               
               (restart-maze h w)
               (dump-text-window playground-window (draw-maze))
               (dump-text-window message-window "Press [Space] key to start."))
             (key-input ()
               (let* ((code (ncurses-getch))
                      (ch (if (and (< code 256) (>= code 0))
                              (code-char code))))
                 (unless (or (eql #\q ch) (eql #\Q ch) (eql #\esc ch))
                   (cond ((let ((dir (cond
                                       ((case ch
                                          (#\w 'w) (#\a 'a) (#\s 's) (#\d 'd)
                                          (#\W 'w) (#\A 'a) (#\S 's) (#\D 'd)))
                                       ((eql +key-up+ code) 'w)
                                       ((eql +key-left+ code) 'a)
                                       ((eql +key-down+ code) 's)
                                       ((eql +key-right+ code) 'd))))
                            ;(dump-text-window tips-window (format nil "~a." code))
                            (let ((user-ij (funcall user-handler dir)))
                              (if user-ij
                                  (progn
                                    (funcall patch-maze (car user-ij)
                                             (1+ (* 2 (cadr user-ij))) "O")
                                    (if (eq-ij-p user-ij
                                                 `(,(1- *H-maze*) ,(1- *W-maze*)))
                                        (re-game-check)
                                        (key-input))
                                    t))))) ; 确保结果一定为真，短路其它分支
                         ((case ch (#\r t) (#\R t))
                          (re-shape))
                         (t (key-input))))))
             (re-game-check ()
               (dump-text-window message-window "You win. Another game? [y/n]")
               (let* ((code (ncurses-getch))
                      (ch (if (and (< code 256) (>= code 0))
                              (code-char code))))
                 (unless (or (eql #\n ch) (eql #\N ch))
                   (cond ((or (eql #\y ch) (eql #\Y ch))
                          ;; 再次开始
                          (game-on h-maze w-maze)
                          (key-input))
                           (t (re-game-check))))))
             (re-shape ()
               "调整迷宫的大小"
               (let ((h (enter-number input-window))
                     (w (enter-number input-window)))
                 (cond
                   ((or (< h 2) (< w 2))
                    (dump-text-window message-window
                                      "height and width share be at least 2. ")
                    (re-shape))
                   ((or (> h 50) (> w 50))
                    (dump-text-window message-window
                                      "height and width share be at most 50. ")
                    (re-shape))
                   (t ; 需要调和尺寸修改后的矛盾
                    ; 需要拓展game-on的语法规则，并且在环境中增加迷宫尺寸
                    (game-on h w) (key-input))))))
      (game-on 20 20)
      (key-input)))
  (ncurses-endwin))
