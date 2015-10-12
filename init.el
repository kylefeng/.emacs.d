(add-to-list 'load-path "~/.emacs.d/custom")
(load "my-package.el")
(load "my-common-setup.el")
(load "my-auto-complete.el")
(load "my-emmet-mode.el")
(load "my-clojure.el")
(load "my-web.el")
(load "my-haskell.el")
(load "my-groovy.el")
(load "my-scheme.el")
(load "my-org.el")
(load "my-others.el")
(load "my-ocaml.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(org-agenda-files (quote ("~/org/test.org"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:foreground "#030303" :background "#bdbdbd" :box nil))))
 '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#666666" :box nil)))))
