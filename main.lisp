
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

(let ((win-mutex (bt:make-lock "window")))
  (defun dump-text-window (win text)
    (bt:with-lock-held (win-mutex)
      (ncurses-wclear win)
      (ncurses-waddstr win text)
      (ncurses-wrefresh win)))
  (defun register-patch-to-window (win)
    (let (char-last y-last x-last)
      (lambda (y x str)
        (bt:with-lock-held (win-mutex)
          (when char-last
            (ncurses-mvwaddstr win y-last x-last (string char-last)))
          (cffi:with-foreign-object (wchar '(:struct cchar-t))
            (ncurses-mvwin-wch win y x wchar)
            (setf char-last (code-char (cffi:foreign-slot-value
                                        wchar '(:struct cchar-t) 'chars)))
            (setf y-last y) (setf x-last x))
          (ncurses-mvwaddstr win y x str)
          (ncurses-wrefresh win))))))

(defun enter-number (win &optional (number 0))
  (dump-text-window win (format nil "~a" number))
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

(defun start-timer (interval call-back)
  "构造一个计时器线程，返回线程，立即执行"
  (let ((time-total 0))
    (bt:make-thread
     (lambda ()
       (loop
         (funcall call-back time-total)
         (sleep interval)
         (setf time-total (+ time-total interval)))))))

(defun init-fun ()
  (init-TUI)
  
  (let (playground-window timerun-window message-window input-window tips-window
        user-handler patch-maze h-maze w-maze thread-timer)
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
               ;(dump-text-window timerun-window "time: 00:00.00")
               (setf thread-timer
                     (start-timer
                      0.07 #'(lambda (time) ; 作为妥协考虑，体现最后一位的滚动
                               (multiple-value-bind (min s) (truncate time 60)
                                 (multiple-value-bind (s raw-cent-s) (truncate s 1)
                                   (let ((cent-s (floor (* 100 raw-cent-s))))
                                     (dump-text-window
                                      timerun-window
                                      (format nil "time: ~2,'0d:~2,'0d.~2,'0d"
                                              min s cent-s))))))))
               (dump-text-window message-window "Now find the 'X' mark.")
               (dump-text-window
                tips-window
                (format nil "[W] for up; ~%[A] for left; ~@
                             [S] for down; ~%[D] for right; ~@
                             [q] to leave the game. ~@
                             [r] to reshape the maze and restart the game. "))
               
               (restart-maze h w)
               (dump-text-window playground-window (draw-maze)))
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
                                        (progn ; 游戏结束(胜利)
                                          (bt:destroy-thread thread-timer)
                                          (re-game-check))
                                        (key-input))
                                    t))))) ; 确保结果一定为真，短路其它分支
                         ((case ch (#\r t) (#\R t))
                          (bt:destroy-thread thread-timer)
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
               (let ((h (progn
                          (dump-text-window message-window "Enter 2 numbers. (now the height)")
                          (enter-number input-window)))
                     (w (progn
                          (dump-text-window message-window "Now the width.")
                          (enter-number input-window))))
                 (cond
                   ((or (< h 2) (< w 2))
                    (dump-text-window message-window
                                      "share be at least 2. [Any] key to continue.")
                    (ncurses-getch) (re-shape))
                   ((or (> h 50) (> w 50))
                    (dump-text-window message-window
                                      "share be at most 50. [Any] key to continue.")
                    (ncurses-getch) (re-shape))
                   (t ; 需要调和尺寸修改后的矛盾
                    ; 需要拓展game-on的语法规则，并且在环境中增加迷宫尺寸
                    (game-on h w) (key-input))))))
      (game-on 20 20)
      (key-input)))
  (ncurses-endwin))
