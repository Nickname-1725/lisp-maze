
;;;; 存储迷宫结构的数据结构

; 1. 双层列表嵌套
; 2. 外层列表作为迷宫的行，内层列表作为迷宫的列
; 3. 内层列表内的列表作为迷宫的节点
; 4. 迷宫节点的数据结构定义为
;    (prev (dir1 dir2 dir2))
;    1) 其中prev表示上一个节点，即由何方位的相邻节点到达该节点
;    2) 如果是迷宫生成的起始点，则prev 为'nil
;    3) dir1, dir2, dir3表示下一节点，即向何方位到达相通的相邻节点
;    4) 方位均由'w, 'a, 's, 'd表示

(defparameter *W-maze* 5)
(defparameter *H-maze* 5)

(defun generate-table ()
  "构造一个*H-maze*行、*W-maze*列的迷宫"
  (let ((table nil))
    (dotimes (i *H-maze*)
      (let ((single-row nil))
        (dotimes (j *W-maze*)
          (setq single-row (cons '(nil ()) single-row)))
        (setq table (cons single-row table))))
    table))

(defparameter *table-maze* (generate-table))

(defmacro get-node (i-j)
  "根据下标访问元素。根据(i j)坐标访问第i行、第j列节点"
  `(nth (nth 1 ,i-j) (nth (nth 0 ,i-j) *table-maze*)))

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
         (next (get-node next-i-j)))
    (or (member dir (nth 1 (get-node i-j)))
        (member (opposite-dir dir) (nth 1 next)))))

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
                          "判断相邻节点合法性，且必须尚未连通"
                          (if (and (i-j-legal-p next-i-j)
                                   (not (link-p i-j dir)))
                              (progn
                                ;给当前节点标记可行方位；给next节点标记prev方位"
                                (setf (nth 1 (get-node i-j)) (cons dir (nth 1 (get-node i-j))))
                                (setf (nth 0 (get-node next-i-j)) (opposite-dir dir))
                                (print next-i-j)
                                ;继续探索下一节点"
                                (explore next-i-j))
                              nil)))
               ;"在穷尽所有可能的方位后，选择退回上一节点"
               ;'(do somethin else)
               )))
    (if (i-j-legal-p init-i-j)
        (progn
          (setf (get-node init-i-j) '(nil ()))
          (explore init-i-j))
        nil)))



