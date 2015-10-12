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
(add-hook 'cider-mode-hook #'eldoc-mode)
(add-hook 'cider-repl-mode-hook #'eldoc-mode)

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

;; Auto-completion
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)
