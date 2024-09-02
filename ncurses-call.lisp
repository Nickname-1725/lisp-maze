
(cffi:define-foreign-library ncurses
  (:unix (:or "/usr/lib/x86_64-linux-gnu/libncurses.so.6"
              "/usr/lib/x86_64-linux-gnu/libncurses.so"))
  (t (:default "libncurses")))

(cffi:use-foreign-library ncurses)

(cffi:defcenum color
  (color-black 0) (color-red 1) (color-green 2) (color-yellow 3)
  (color-blue 4) (color-magenta 5) (color-cyan 6) (color-white 7))
(cffi:defcenum texture-pair
  (grass-pair 1) (empty-pair 1) (water-pair 2) (mountain-pair 3) (player-pair 4))
(cffi:defcenum local-category
  (lc-all 0) (lc-collate 1) (lc-ctype 2) (lc-monetary 3) (lc-numeric 4) (lc-time 5))

(cffi:defcfun ("initscr" ncurses-initscr) :pointer)
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

(cffi:defcfun ("COLOR_PAIR" ncurses-color-pair) :int (pair-number :int))
(cffi:defcfun ("attron" ncurses-attron) :int (at :int))
(cffi:defcfun ("attroff" ncurses-attroff) :int (at :int))
(cffi:defcfun ("mvhline" ncurses-mvhline) :int (y :int) (x :int) (ch :char) (n :int))

(cffi:defcfun ("getch" ncurses-getch) :char)

(defparameter *stdscr* nil)

(defun draw-map ()
  ;(let (y x))
  (ncurses-attron (ncurses-color-pair grass-pair))
  (ncurses-mvhline 2 0 (char-code #\space) 20)
  (ncurses-attroff (ncurses-color-pair grass-pair)))

(defun key-input ()
  (let ((ch (ncurses-getch)))
    (cond ((eql (char-code #\q) ch)
           nil)
          (t (key-input)))))

(defun init-fun* ()
  (ncurses-setlocale lc-all "")
  (setf *stdscr* (ncurses-initscr))
  (ncurses-cbreak)
  (ncurses-noecho)
  (ncurses-keypad *stdscr* t)
  
  ;; 初始化颜色
  (ncurses-start-color)
  (ncurses-init-pair grass-pair color-yellow color-green)
  (ncurses-init-pair water-pair color-cyan color-blue)
  (ncurses-init-pair mountain-pair color-black color-white)
  (ncurses-init-pair player-pair color-red color-magenta)
  
  (draw-map)
  (ncurses-refresh)
  
  (key-input)
  (ncurses-endwin))
