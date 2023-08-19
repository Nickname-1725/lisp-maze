;;;; 迷宫节点的数据结构

; 迷宫节点的数据结构定义为
; (prev (dir1 dir2 dir2))
; 1. 其中prev表示上一个节点，即由何方位的相邻节点到达该节点
; 2. 如果是迷宫生成的起始点，则prev 为'nil
; 3. dir1, dir2, dir3表示下一节点，即向何方位到达相通的相邻节点
; 4. 方位均由'w, 'a, 's, 'd表示

(defun Create-node ()
  (copy-list '(:prev nil :dir-list ())))
(defmacro get-node-key (node key)
  `(getf ,node ,key))
(defun set-node-prev (node value)
  (setf (get-node-key node :prev) value))
(defun add-node-dir-list (node new-dir)
  (setf (get-node-key node :dir-list)
        (cons new-dir (get-node-key node :dir-list))))

;;;; 存储迷宫结构的数据结构

; 1. 双层列表嵌套
; 2. 外层列表作为迷宫的行，内层列表作为迷宫的列
; 3. 内层列表内的列表作为迷宫的节点

(defparameter *W-maze* 5)
(defparameter *H-maze* 5)

(defun generate-table ()
  "构造一个*H-maze*行、*W-maze*列的迷宫"
  (let ((table (make-array `(,*H-maze* ,*W-maze*) :initial-element (Create-node))))
    (dotimes (i *H-maze*)
      (dotimes (j *W-maze*)
        (setf (aref table i j) (Create-node))))
    table))

(defparameter *table-maze* (generate-table))

(defmacro get-node (i-j)
  "根据下标访问元素。根据(i j)坐标访问第i行、第j列节点"
  `(aref ,*table-maze* (nth 0 ,i-j) (nth 1 ,i-j)))

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
  "节点(i j)向dir方向是否连通（不判断相邻节点的合法性）"
  (let* ((next-i-j (next-node i-j dir))
         (next (get-node next-i-j))
         (current (get-node i-j)))
    (or (member dir (get-node-key current :dir-list))
        (member (opposite-dir dir) (get-node-key next :dir-list)))))

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
                                      ;添加已探所节点
                                      (done-node next)
                                      ;继续探索下一节点"
                                      (explore next-i-j)))
                              nil))))))
    (if (i-j-legal-p init-i-j)
        (progn
          (explore init-i-j))
        nil)))



