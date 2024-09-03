;;;; 迷宫节点的数据结构

(defun create-node ()
  "prev表示前继节点，由何方向相邻节点到达该节点
   dir-list表示后继节点列表，自该节点可到达何方向诸节点
   方向均由键盘按键'w, 'a, 's, 'd表示"
  (copy-list '(:prev nil :dir-list ())))
(defmacro get-node-key (node key)
  `(getf ,node ,key))
(defun set-node-prev (node value)
  (setf (get-node-key node :prev) value))
(defun add-node-dir-list (node new-dir)
  (setf (get-node-key node :dir-list)
        (cons new-dir (get-node-key node :dir-list))))

;;;; 存储迷宫结构的数据结构

(defparameter *W-maze* 2)
(defparameter *H-maze* 2)

(defun generate-table ()
  "构造一个*H-maze*行、*W-maze*列的迷宫；内部储存节点"
  (let ((table (make-array `(,*H-maze* ,*W-maze*) :initial-element (create-node))))
    (dotimes (i *H-maze*)
      (dotimes (j *W-maze*)
        (setf (aref table i j) (create-node))))
    table))

(defparameter *table-maze*  nil)

(defmacro get-node (i-j)
  "根据下标访问元素。根据(i j)坐标访问第i行、第j列节点"
  `(aref *table-maze* (nth 0 ,i-j) (nth 1 ,i-j)))

(defun neibour-node (i-j dir step)
  "从节点(i j)出发，向dir方向行走step布"
  (let* ((i (nth 0 i-j))
         (j (nth 1 i-j))
         (i-next i)
         (j-next j))
    (cond ((eql dir 'w) (setq i-next (- i step)))
          ((eql dir 'a) (setq j-next (- j step)))
          ((eql dir 's) (setq i-next (+ i step)))
          ((eql dir 'd) (setq j-next (+ j step))))
    `(,i-next ,j-next)))
(defun next-node (i-j dir) (neibour-node i-j dir 1))
(defun last-node (i-j dir) (neibour-node i-j dir -1))

(defun opposite-dir (dir)
  (cond ((eql dir 'w) 's)
        ((eql dir 'a) 'd)
        ((eql dir 's) 'w)
        ((eql dir 'd) 'a)))

(defun link-p (i-j dir)
  "节点(i j)向dir方向是否连通（若相邻节点的不合法，则认为不连通）"
  (let ((next-i-j (next-node i-j dir)))
    (if (i-j-legal-p next-i-j)
        (let ((next (get-node next-i-j))
              (current (get-node i-j)))
          (or (member dir (get-node-key current :dir-list))
              (member (opposite-dir dir) (get-node-key next :dir-list))))
        nil)))

;;;; 存储已开启节点、待探索边界的数据结构
(defun create-todo-edge (i-j dir)
  "定义待探索edge的数据结构，以构造函数的形式表示
   存储坐标为(i j)的节点指向dir方向的edge"
  `(:node-i-j ,i-j :dir ,dir))

(defparameter *done-node-list* nil)
(defparameter *todo-edge-list* nil)

(defun done-node-p (node)
  (find node *done-node-list*))
(defmacro done-node (node)
  "操作：添加已开启node（无需移除）"
  `(push ,node *done-node-list*))

(defun push-todo (i-j dir)
  "操作：添加待探索edge"
  (cons (create-todo-edge i-j dir) *todo-edge-list*))
(defun pop-todo ()
  "操作：弹出待探索edge"
  (let ((todo-edge (car *todo-edge-list*)))
    (setf *todo-edge-list* (cdr *todo-edge-list*))
    todo-edge))

;;;; 迷宫的生成算法
(defun i-j-legal-p (i-j)
  (let ((i (nth 0 i-j))
        (j (nth 1 i-j)))
    (and (>= i 0) (< i *H-maze*)
         (>= j 0) (< j *W-maze*))))
(defun init-maze (init-i-j)
  (labels ((todo-list () "创建即将探索的各方位的列表"
             (let ((todo nil))
               (loop
                 (let* ((key (random 4))
                        (dir (cond ((= key 0) 'w)
                                   ((= key 1) 'a)
                                   ((= key 2) 's)
                                   ((= key 3) 'd))))
                   (if (member dir todo) nil
                       (setq todo (cons dir todo)))
                   "当todo列表填满第4个方位时，返回该列表"
                   (when (nth 3 todo) (return todo))))))
           (explore (i-j)
             (done-node (get-node i-j)) ; 将当前节点标记为已开启
             (let ((todo (todo-list)))
               (loop for dir in todo
                     do (let ((next-i-j (next-node i-j dir)))
                          ;判断相邻节点合法性，且必须尚未开启
                          (if (and (i-j-legal-p next-i-j)
                                   (not (done-node-p (get-node next-i-j))))
                              (progn(let ((next (get-node next-i-j))
                                          (current (get-node i-j)))
                                       ;给当前节点标记可行方位；给next节点标记prev方位"
                                      (add-node-dir-list current dir)
                                      (set-node-prev next (opposite-dir dir))
                                      ;继续探索下一节点"
                                      (explore next-i-j)))
                              nil))))))
    (if (i-j-legal-p init-i-j)
        (progn
          (explore init-i-j))
        nil)))

;;;; 迷宫的渲染
(defparameter *user-ij* '(0 0))
(defun eq-ij-p (ij-1 ij-2)
  (and (eq (car ij-1) (car ij-2))
       (eq (cadr ij-1) (cadr ij-2))))
(defun draw-maze ()
  (flet ((draw-crossing (i-j)
           (let ((enum 0))
             (when (link-p i-j 'w) (setf enum (+ enum (ash 1 0))))
             (when (link-p i-j 's) (setf enum (+ enum (ash 1 1))))
             (when (link-p i-j 'a) (setf enum (+ enum (ash 1 2))))
             (when (link-p i-j 'd) (setf enum (+ enum (ash 1 3))))
             (let ((crossing-list '("O " "╙ " "╓ " "║ "
                                    "═ " "╝ " "╗ " "╣ "
                                    " ═" "╚═" "╔═" "╠═"
                                    "══" "╩═" "╦═" "╬═"
                                    "X ")))
               (cond ((eq-ij-p i-j *user-ij*)
                      (car crossing-list))
                     ((eq-ij-p i-j `(,(1- *H-maze*) ,(1- *W-maze*)))
                      (nth 16 crossing-list))
                     (t (nth enum crossing-list)))))))
    (let ((text-maze ""))
      (dotimes (i *H-maze*)
        (let ((text-row " "))
          (dotimes (j *W-maze*)
            (setf text-row (concatenate 'string  text-row (draw-crossing `(,i ,j)))))
          (setf text-maze (concatenate 'string text-maze text-row))))
      text-maze)))

;;;; REPL
(defparameter *last-dir* nil)
(defun restart-maze (height width)
  (setf *user-ij* '(0 0))
  (setf *H-maze* height)
  (setf *W-maze* width)
  (setf *table-maze* (generate-table))
  (init-maze `(,(ash *H-maze* -1) ,(ash *W-maze* -1))))
(defun move-user (dir)
  (let ((usr-ij (copy-list *user-ij*)))
    (cond ((eq dir 'w) (incf (car usr-ij) -1))
          ((eq dir 'a) (incf (cadr usr-ij) -1))
          ((eq dir 's) (incf (car usr-ij) 1))
          ((eq dir 'd) (incf (cadr usr-ij) 1))
          (t nil))
    (if (and (i-j-legal-p usr-ij)
             (link-p *user-ij* dir))
        (progn
          (setf *user-ij* usr-ij)
          (setf *last-dir* dir)))))
(defun information-maze ()
  (format t "~c[2J~c[H" #\escape #\escape)
  (format t "[W] for up; ~%[A] for left; ~%[S] for down; ~%[D] for right; ~%")
  (format t "Directions can be input without a break. ~%")
  (format t "You last direction is remembered. ~%")
  (format t "[q] to leave the game. ~%")
  (format t "[r] to reshape the maze and restart the game. ~%")
  (format t "~a~%" (draw-maze))
  (if (eq-ij-p *user-ij* `(,(1- *H-maze*) ,(1- *W-maze*)))
      (labels ((repl-loop ()
                 (format t "You win. Another game? [y/n]~%")
                 (let* ((cmd-string (string-trim " " (read-line)))
                        (cmd (if (eq 0 (length cmd-string))
                                 nil (read-from-string cmd-string))))
                   (cond ((eq cmd 'y) (restart-maze *H-maze* *W-maze*)(repl))
                         ((eq cmd 'n) nil)
                         (t (repl-loop))))))
        (repl-loop))))

(defun repl()
  (information-maze)
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
          (t (if cmd-list
                 (mapcar #'move-user cmd-list)
                 (move-user *last-dir*))
             (repl)))))

(defun init-gameboard ()
  (let* ((side-bar-width 25)
         (playground-size-y 20)
         (playground-size-x (1+ (* 2 20)))
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
            game-board-windows 2 side-bar-width 1 playground-size-x))
         (tips-window
           (ncurses-derwin
            game-board-windows (- playground-size-y 1 2)
            side-bar-width 3 playground-size-x)))
    (ncurses-wbkgd playground-window (ncurses-color-pair playground-pair))
    (ncurses-wbkgd timerun-window (ncurses-color-pair menu-light-pair))
    (ncurses-wbkgd message-window (ncurses-color-pair menu-dark-pair))
    (ncurses-wbkgd tips-window (ncurses-color-pair menu-light-pair))
    (values playground-window timerun-window message-window tips-window)))

(defun dump-text-window (win text)
  (ncurses-wclear win)
  (ncurses-waddstr win text)
  (ncurses-wrefresh win))

(defun init-fun ()
  (init-TUI)
  
  (restart-maze 20 20)
  (multiple-value-bind (playground-window timerun-window message-window tips-window)
      (init-gameboard)
    (dump-text-window playground-window (draw-maze))
    (dump-text-window timerun-window "time: 00:00.00")
    (dump-text-window message-window "Press [Space] key to start.")
    (dump-text-window tips-window "[q/Q] for quit.")
    
    (labels ((key-input ()
               (let* ((code (ncurses-getch))
                      (ch (if (and (< code 256) (>= code 0))
                              (code-char code))))
                 (unless (or (eql #\q ch) (eql #\Q ch) (eql #\esc ch))
                   (cond ((let ((dir (cond
                                       (ch ch)
                                       ((eql +key-up+ code) 'w)
                                       ((eql +key-left+ code) 'a)
                                       ((eql +key-down+ code) 's)
                                       ((eql +key-right+ code) 'd))))
                            (dump-text-window tips-window (format nil "~a." code))
                            (move-user (read-from-string (string dir))))
                          (dump-text-window playground-window (draw-maze)))
                         (t nil))
                   (key-input)))))
      (ncurses-refresh)
      (key-input)))
  (ncurses-endwin))
