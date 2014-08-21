;; auto-complete
(require 'auto-complete-config)

(add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")

(ac-config-default)
(add-to-list 'ac-modes 'enh-ruby-mode)
(add-to-list 'ac-modes 'web-mode)

