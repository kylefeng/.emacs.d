;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CoffeeScript

(custom-set-variables '(coffee-tab-width 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; HTML mode

(add-hook 'html-mode-hook
          (lambda ()
            (setq sgml-basic-offset 4)
            (setq indent-tabs-mode t)))
