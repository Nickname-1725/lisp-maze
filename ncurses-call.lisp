
(cffi:define-foreign-library ncurses
  (:unix (:or "/usr/lib/x86_64-linux-gnu/libncursesw.so.6"
              "/usr/lib/x86_64-linux-gnu/libncursesw.so"))
  (t (:default "libncursesw")))

(cffi:use-foreign-library ncurses)

(cffi:defcenum color
  (color-black 0) (color-red 1) (color-green 2) (color-yellow 3)
  (color-blue 4) (color-magenta 5) (color-cyan 6) (color-white 7))
(cffi:defcenum texture-pair
  (playground-pair 1) (menu-light-pair 2) (menu-dark-pair 3) (input-pair 4))
(cffi:defcenum local-category
  (lc-all 0) (lc-collate 1) (lc-ctype 2) (lc-monetary 3) (lc-numeric 4) (lc-time 5))

(defconstant +key-up+ 259)
(defconstant +key-down+ 258)
(defconstant +key-left+ 260)
(defconstant +key-right+ 261)

(cffi:defcfun ("initscr" ncurses-initscr) :pointer)
; curs_set: visibility: 光标可见性
; - 0: 隐藏
; - 1: 可见
; - 2: 闪烁 
(cffi:defcfun ("curs_set" ncurses-curs-set) :int (visibility :int))
(cffi:defcfun ("setlocale" ncurses-setlocale) :string
  (category local-category) (locale :string))
(cffi:defcfun ("cbreak" ncurses-cbreak) :void)
(cffi:defcfun ("noecho" ncurses-noecho) :void)
(cffi:defcfun ("keypad" ncurses-keypad) :pointer (win :pointer) (enable :bool))
(cffi:defcfun ("start_color" ncurses-start-color) :void)
(cffi:defcfun ("init_pair" ncurses-init-pair) :void
  (pair :int) (text-color :int) (background-color :int))
(cffi:defcfun ("clear" ncurses-clear) :void)
(cffi:defcfun ("refresh" ncurses-refresh) :void)
(cffi:defcfun ("endwin" ncurses-endwin) :void)

(cffi:defcfun ("newwin" ncurses-newwin) :pointer
  (num-of-lines :int) (num-of-cols :int) (start-y :int) (start-x :int))
(cffi:defcfun ("delwin" ncurses-delwin) :int (window-to-delete :pointer))
(cffi:defcfun ("derwin" ncurses-derwin) :pointer
  (win :pointer) (lines :int) (cols :int)  (y :int) (x :int))
(cffi:defcfun ("wbkgd" ncurses-wbkgd) :int (win :pointer) (character texture-pair))
(cffi:defcfun ("wclear" ncurses-wclear) :int (win :pointer))
(cffi:defcfun ("waddstr" ncurses-waddstr) :int (win :pointer) (str :string))
(cffi:defcfun ("mvwaddstr" ncurses-mvwaddstr) :int
  (win :pointer) (y :int) (x :int) (str :string))
(cffi:defcfun ("wmove" ncurses-wmove) :int (win :pointer) (y :int) (x :int))
(cffi:defcfun ("wrefresh" ncurses-wrefresh) :int (win :pointer))
(cffi:defcfun ("mvwin_wch" ncurses-mvwin-wch) :int
  (win :pointer) (y :int) (x :int) (wch :pointer))
(cffi:defcstruct cchar-t (attr :uint32) (chars :uint32))

(cffi:defcfun ("COLOR_PAIR" ncurses-color-pair) :int (pair-number :int))
(cffi:defcfun ("attron" ncurses-attron) :int (at :int))
(cffi:defcfun ("attroff" ncurses-attroff) :int (at :int))
(cffi:defcfun ("mvhline" ncurses-mvhline) :int (y :int) (x :int) (ch :char) (n :int))

(cffi:defcfun ("getch" ncurses-getch) :int)

(cffi:defcvar ("LINES" ncurses-lines) :int)
(cffi:defcvar ("COLS" ncurses-cols) :int)

(defparameter *stdscr* nil)

(defun init-TUI ()
  (ncurses-setlocale lc-all "")
  (setf *stdscr* (ncurses-initscr))
  (ncurses-curs-set 0)
  (ncurses-cbreak)
  (ncurses-noecho)
  (ncurses-keypad *stdscr* t)
  
  ;; 初始化颜色
  (ncurses-start-color)
  (ncurses-init-pair playground-pair color-black color-white)
  (ncurses-init-pair menu-light-pair color-black color-cyan)
  (ncurses-init-pair menu-dark-pair color-white color-blue)
  (ncurses-init-pair input-pair color-white color-black)
  
  ;(draw-map)
  (ncurses-refresh))
