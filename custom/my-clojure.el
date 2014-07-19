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

;; ac-nrepl
(require 'ac-nrepl)
(add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
(add-hook 'cider-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-repl-mode))

(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

(add-hook 'cider-repl-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)

(eval-after-load "cider"
  '(define-key cider-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc))
