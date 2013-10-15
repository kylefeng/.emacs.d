;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; global configs

(global-linum-mode t)

;; Close *Gnu Emacs* Buffer
(setq inhibit-startup-screen t)

(setq visible-bell t)
(setq x-select-enable-clipboard t)
(ispell-change-dictionary "american" t)
(setq default-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Display

;; Turn off tool bar
(tool-bar-mode nil)

(show-paren-mode t)
(setq show-paren-style 'parentheses)
(defun paredit-mode-enable () (paredit-mode 1))
(add-hook 'clojure-mode-hook 'paredit-mode-enable)
(add-hook 'clojure-mode-hook 'highlight-parentheses-mode)
(add-hook 'clojure-test-mode-hook 'paredit-mode-enable)

;; Size of window
(setq default-frame-alist
  '((height . 40)
    (width  . 125)
    (menu-bar-lines . 20)
    (tool-bar-lines . 0)))

;; Display column number
(setq column-number-mode t)
(setq line-number-mode t)

;; Color-theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/color-theme/emacs-color-theme-solarized")
(add-to-list 'load-path "~/.emacs.d/color-theme/tomorrow-theme")
(require 'color-theme-tomorrow)
(color-theme-tomorrow-night-bright)

;; Powerline
(add-to-list 'load-path "~/.emacs.d/powerline")
(require 'powerline)
(powerline-default)
(setq powerline-arrow-shape 'arrow14)
(custom-set-faces
 '(mode-line ((t (:foreground "#030303" :background "#bdbdbd" :box nil))))
 '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#666666" :box nil)))))

;; io-mode
(add-to-list 'load-path "~/.emacs.d/io-mode")
(require 'io-mode)
(setq auto-mode-alist
      (cons '("\\.io" . io-mode) auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Files

(setq backup-by-copying t
  backup-directory-alist '(("." . "~/.saves")) 
  delete-old-versions t                        
  kept-new-version 6                           
  kept-old-version 2                           
  version-control t)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Hippie Expand
(global-set-key (kbd "M-/") 'hippie-expand)

(setq hippie-expand-try-functions-list
      '(try-expand-all-abbrevs try-expand-dabbrev
        try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially try-complete-lisp-symbol
        try-complete-file-name-partially try-complete-file-name))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Package

;; Marmalade
(require 'package)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages 
  '(starter-kit
    slime
    slime-repl
    clojure-mode
    clojurescript-mode
    nrepl
    coffee-mode
    erlang
    auto-complete
    markdown-mode
    highlight-parentheses
    yaml-mode
    vline
    hl-line+
    col-highlight
    crosshairs
    haskell-mode
    sml-mode)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; crosshair highlighting
(toggle-crosshairs-when-idle)


;; auto-complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete-1.4/dict/")
(ac-config-default)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Markdown

(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))

(add-hook 'markdown-mode-hook
          'turn-off-auto-fill)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Erlang


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Clojure

'(eval-after-load "clojure-mode"
  '(progn
    (require 'slime)
    (require 'clojure-mode)
    (unless (slime-connected-p)
      (save-excursion (nrepl-jack-in)))
    (setq slime-net-coding-system 'utf-8-unix)))

;; Hide special buffers
(setq nrepl-hide-special-buffers t)

;; Enable eldoc in clojure buffers
(add-hook 'nrepl-interaction-mode-hook
          'nrepl-turn-on-eldoc-mode)

;; Stop popping up error buffer other than the REPL
(setq nrepl-popup-stacktraces nil)

;; Enable error buffer popping also in the REPL
(setq nrepl-popup-stacktraces-in-repl t)



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

(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(add-hook 'slime-repl-mode-hook
          (lambda ()
            (font-lock-mode nil)
            (clojure-font-lock-setup)
            (font-lock-mode t)))


(add-hook 'nrepl-mode-hook
          (lambda ()
            (font-lock-mode nil)
            (clojure-font-lock-setup)
            (font-lock-mode t)
            (remove-dos-eol)
            (subword-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CoffeeScript
(custom-set-variables '(coffee-tab-width 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; HTML mode

(add-hook 'html-mode-hook
          (lambda ()
            (setq sgml-basic-offset 4)
            (setq indent-tabs-mode t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Haskell Mode

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
