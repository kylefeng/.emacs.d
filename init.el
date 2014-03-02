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


(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

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
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages 
  '(starter-kit
    starter-kit-lisp
    starter-kit-bindings
    starter-kit-eshell
    clojure-mode
    clojure-test-mode
    cider
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
    soft-morning-theme
    json-mode)
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

(add-hook 'text-mode-hook
          (lambda () (turn-on-auto-fill 0)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Markdown

(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))

(add-hook 'markdown-mode-hook
          'turn-off-auto-fill)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Clojure

;; Paredit-mode
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'remove-dos-eol)
(add-hook 'cider-repl-mode-hook 'highlight-parentheses-mode)
(add-hook 'cider-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)

;; Enable eldoc
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

;; Hide *nrepl-connnection* and *nrepl-server* buffer
(setq nrepl-hide-special-buffers t)

;; Stop the error buffer from popping up while working in buffers other than the REPL:
(setq cider-popup-stacktraces nil)

;; Enable error buffer popping also in the REPL:
(setq cider-repl-popup-stacktraces t)

;; To auto-select the error buffer when it's displayed:
(setq cider-auto-select-error-buffer t)

;; Buffer name will look like cider project-name:port.
(setq nrepl-buffer-name-show-port t)

;; Make C-c C-z switch to the CIDER REPL buffer in the current window:
(setq cider-repl-display-in-current-window nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CoffeeScript

(custom-set-variables '(coffee-tab-width 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; HTML mode

(add-hook 'html-mode-hook
          (lambda ()
            (setq sgml-basic-offset 4)
            (setq indent-tabs-mode t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Haskell Mode

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; org-mode
(setq org-log-done t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; scheme

(require 'cmuscheme)
(setq scheme-program-name "racket")

(defun scheme-proc ()
  "Return the current Scheme process, starting one if necessary."
  (unless (and scheme-buffer
               (get-buffer scheme-buffer)
               (comint-check-proc scheme-buffer))
    (save-window-excursion
      (run-scheme scheme-program-name)))
  (or (scheme-get-process)
      (error "No current process. See variable `scheme-buffer'")))

(defun scheme-split-window ()
  (cond
   ((= 1 (count-windows))
    (delete-other-windows)
    (split-window-vertically (floor (* 0.68 (window-height))))
    (other-window 1)
    (switch-to-buffer "*scheme*")
    (other-window 1))
   ((not (find "*scheme*"
               (mapcar (lambda (w) (buffer-name (window-buffer w)))
                       (window-list))
               :test 'equal))
    (other-window 1)
    (switch-to-buffer "*scheme*")
    (other-window -1))))

(defun scheme-send-last-sexp-split-window ()
  (interactive)
  (scheme-split-window)
  (scheme-send-last-sexp))

(defun scheme-send-definition-split-window ()
  (interactive)
  (scheme-split-window)
  (scheme-send-definition))

(add-hook 'scheme-mode-hook
  (lambda ()
    (paredit-mode 1)
    (define-key scheme-mode-map (kbd "C-c C-e") 'scheme-send-last-sexp-split-window)
    (define-key scheme-mode-map (kbd "C-c C-l") 'scheme-send-definition-split-window)))

