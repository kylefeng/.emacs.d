;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Clojure
(require 'cider)

(setq cider-repl-use-clojure-font-lock t)
(add-hook 'clojure-mode-hook 'cider-mode)

;; Paredit-mode
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)

(setq cider-repl-tab-command 'cider-repl-indent-and-complete-symbol)

;; remove dos eol
(add-hook 'cider-repl-mode-hook 'remove-dos-eol)

;; highlight parentheses
(add-hook 'cider-repl-mode-hook 'highlight-parentheses-mode)
(add-hook 'cider-mode-hook 'highlight-parentheses-mode)

;; Enable eldoc
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-repl-mode-hook 'cider-turn-on-eldoc-mode)

;; Hide *nrepl-connnection* and *nrepl-server* buffer
(setq nrepl-hide-special-buffers t)

;; Error buffer with stacktraces
(setq cider-show-error-buffer nil)

;; Enable error buffer popping also in the REPL:
(setq cider-repl-popup-stacktraces t)

;; To auto-select the error buffer when it's displayed:
(setq cider-auto-select-error-buffer nil)

;; Buffer name will look like cider project-name:port.
(setq nrepl-buffer-name-show-port t)

;; Make C-c C-z switch to the CIDER REPL buffer in the current window:
(setq cider-repl-display-in-current-window nil)

;; ac-cider
(require 'ac-cider)
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-mode))
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-repl-mode))

(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))

(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'cider-repl-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)
