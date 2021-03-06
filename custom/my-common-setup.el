;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; global configs
(global-linum-mode t)

;; change scratch init message
(setq initial-scratch-message "")

;; Close *Gnu Emacs* Buffer
(setq inhibit-startup-screen t)

(setq visible-bell nil)
(setq x-select-enable-clipboard t)
(ispell-change-dictionary "american" t)
(setq default-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq tab-width 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Display

;; Font
(when (display-graphic-p) 
  (setq fonts 
        (cond ((eq system-type 'darwin)     '("Monaco"     "STHeiti")) 
              ((eq system-type 'gnu/linux)  '("Menlo"     "WenQuanYi Zen Hei")) 
              ((eq system-type 'windows-nt) '("Consolas"  "Microsoft Yahei")))) 

  (setq face-font-rescale-alist '(("STHeiti" . 1.2) ("Microsoft Yahei" . 1.2) ("WenQuanYi Zen Hei" . 1.2))) 
  (set-face-attribute 'default nil :font 
                      (format "%s:pixelsize=%d" (car fonts) 12)) 
  (dolist (charset '(kana han symbol cjk-misc bopomofo)) 
    (set-fontset-font (frame-parameter nil 'font) charset 
                      (font-spec :family (car (cdr fonts)))))) 


;; Turn off tool bar
(tool-bar-mode nil)
(show-paren-mode t)
(setq show-paren-style 'parentheses)

;; Turn off menu bar
(menu-bar-mode -1)

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


(auto-fill-mode -1)
(remove-hook 'text-mode-hook 'turn-on-auto-fill)

'(add-hook 'text-mode-hook
          (lambda () (turn-on-auto-fill 0)))


;; exec-path-from-shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(setq exec-path (cons "/usr/local/bin" exec-path))

  (when window-system (set-exec-path-from-shell-PATH))

  (cond
   ((eq window-system 'ns)
    (setq shell-command-switch "-lc")))


