;; auto-complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete-1.4/dict/")
(ac-config-default)

(add-hook 'text-mode-hook
          (lambda () (turn-on-auto-fill 0)))
