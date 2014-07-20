;; auto-complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories
             "~/.emacs.d/.cask/24.3.1/elpa/auto-complete-20140618.2217/dict")
(ac-config-default)

(add-hook 'text-mode-hook
          (lambda () (turn-on-auto-fill 0)))
