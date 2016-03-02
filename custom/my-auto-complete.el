;; auto-complete
; (require 'auto-complete-config)

; (add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")

; (ac-config-default)
; (add-to-list 'ac-modes 'enh-ruby-mode)
; (add-to-list 'ac-modes 'web-mode)

(require 'company)
(require 'company-go)

(custom-set-faces
 '(company-preview
   ((t (:foreground "darkgray" :underline t))))
 '(company-preview-common
   ((t (:inherit company-preview))))
 '(company-tooltip
   ((t (:background "lightgray" :foreground "black"))))
 '(company-tooltip-selection
   ((t (:background "steelblue" :foreground "white"))))
 '(company-tooltip-common
   ((((type x)) (:inherit company-tooltip :weight bold))
    (t (:inherit company-tooltip))))
 '(company-tooltip-common-selection
   ((((type x)) (:inherit company-tooltip-selection :weight bold))
    (t (:inherit company-tooltip-selection)))))


(setq company-tooltip-limit 20) ;; bigger popup window
(setq company-idle-delay .3) ;; decrease delay before autocompletion popup shows
(setq company-echo-delay 0) ;; remove annoying blinking
(setq company-begin-commands '(self-insert-command)) ;; start autocompletion only after typing


(add-hook 'after-init-hook 'global-company-mode)
