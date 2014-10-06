(require 'emmet-mode)
(add-hook 'html-mode-hook 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)

(add-hook 'emmet-mode-hook (lambda () (setq emmet-indent-after-insert nil)))

;; indent 2 spaces.
(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2)))

(setq emmet-move-cursor-between-quotes t)
(setq emmet-move-cursor-after-expanding nil)




