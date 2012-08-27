;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 通用设置

;; Close *Gnu Emacs* Buffer
(setq inhibit-startup-screen t)

;; 关闭错误提示音
(setq visible-bell t)

;; 支持外部程序粘贴
(setq x-select-enable-clipboard t)

;; 拼写检查
(ispell-change-dictionary "american" t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 显示设置

;; 关闭工具条
(tool-bar-mode nil)

;; 括号匹配
(show-paren-mode t)
(setq show-paren-style 'parentheses)
(defun paredit-mode-enable () (paredit-mode 1))
(add-hook 'clojure-mode-hook 'paredit-mode-enable)
(add-hook 'clojure-test-mode-hook 'paredit-mode-enable)

;; 窗口大小
(setq default-frame-alist
  '((height . 40)
    (width  . 125)
    (menu-bar-lines . 20)
    (tool-bar-lines . 0)))

;; Display column number
(setq column-number-mode t)
(setq line-number-mode t)

;; Color-theme
(add-to-list 'load-path "~/.emacs.d/color-theme")
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
    (color-theme-initialize)
    (color-theme-charcoal-black)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 文件设置

;; 自动备份设置
(setq backup-by-copying t
  backup-directory-alist '(("." . "~/.saves")) ; 自动备份到 ~/.saves 文件夹
  delete-old-versions t                        ; 自动删除旧的备份文件
  kept-new-version 6                           ; 保留最近的6个备份文件
  kept-old-version 2                           ; 保留最早的2个备份文件
  version-control t)                           ; 多次备份

;; 自动切换到 markdown-mode
(setq auto-mode-alist
      (cons '("\\.text" . markdown-mode) auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; key

;; copy region or whole line
(global-set-key "\M-w"
                (lambda ()
                  (interactive)
                  (if mark-active
                      (kill-ring-save (region-beginning)
                                      (region-end))
                    (progn
                      (kill-ring-save (line-beginning-position)
                                      (line-end-position))
                      (message "copied line")))))


;; kill region or whole line
(global-set-key "\C-w"
                (lambda ()
                  (interactive)
                  (if mark-active
                      (kill-region (region-beginning)
                                   (region-end))
                    (progn
                      (kill-region (line-beginning-position)
                                   (line-end-position))
                      (message "killed line")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Package

;; Marmalade
(add-to-list 'load-path "~/.emacs.d/elpa")
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; 保证所有机器都会安装相应的包
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages 
  '(starter-kit
    slime
    slime-repl
    clojure-mode
    clojurescript-mode
    auto-complete
    markdown-mode)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; auto-complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete-1.4/dict/")
(ac-config-default)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Clojure

;; 打开 clj 文件自动启动 clojuer-jack-in 与 slime 的连接 
(eval-after-load "clojure-mode"
  '(progn
    (require 'slime)
    (require 'clojure-mode)
    (unless (slime-connected-p)
      (save-excursion (clojure-jack-in)))))

;; 启用 IO 重定向，设置通讯编码
(eval-after-load "clojure-mode"
  '(progn
    (require 'slime)
    (setq swank:*globally-redirect-io* t)
    (setq slime-net-coding-system 'utf-8-unix)))

;; slime REPL 支持 Clojure 的语法高亮

;;; all code in this function lifted from the clojure-mode function
;;; from clojure-mode.el
(defun clojure-font-lock-setup ()
  (interactive)
  (set (make-local-variable 'lisp-indent-function)
       'clojure-indent-function)
  (set (make-local-variable 'lisp-doc-string-elt-property)
       'clojure-doc-string-elt)
  (set (make-local-variable 'font-lock-multiline) t)

  (add-to-list 'font-lock-extend-region-functions
               'clojure-font-lock-extend-region-def t)

  (when clojure-mode-font-lock-comment-sexp
    (add-to-list 'font-lock-extend-region-functions
                 'clojure-font-lock-extend-region-comment t)
    (make-local-variable 'clojure-font-lock-keywords)
    (add-to-list 'clojure-font-lock-keywords
                 'clojure-font-lock-mark-comment t)
    (set (make-local-variable 'open-paren-in-column-0-is-defun-start) nil))

  (setq font-lock-defaults
        '(clojure-font-lock-keywords    ; keywords
          nil nil
          (("+-*/.<>=!?$%_&~^:@" . "w")) ; syntax alist
          nil
          (font-lock-mark-block-function . mark-defun)
          (font-lock-syntactic-face-function
           . lisp-font-lock-syntactic-face-function))))

(add-hook 'slime-repl-mode-hook
          (lambda ()
            (font-lock-mode nil)
            (clojure-font-lock-setup)
            (font-lock-mode t)))
