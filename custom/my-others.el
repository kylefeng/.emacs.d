;; io-mode
(add-to-list 'load-path "~/.emacs.d/io-mode")
(require 'io-mode)
(setq auto-mode-alist
      (cons '("\\.io" . io-mode) auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; xml
(setq sgml-basic-offset 4)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Markdown
(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))

(add-hook 'markdown-mode-hook
          'turn-off-auto-fill)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Hippie Expand
(global-set-key (kbd "M-/") 'hippie-expand)

(setq hippie-expand-try-functions-list
      '(try-expand-all-abbrevs try-expand-dabbrev
        try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially try-complete-lisp-symbol
        try-complete-file-name-partially try-complete-file-name))

;; crosshair highlighting
(toggle-crosshairs-when-idle)
