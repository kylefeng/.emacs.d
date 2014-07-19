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


