;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Package

;; Cask
(require 'cask "/usr/local/Cellar/cask/0.7.0/cask.el")
(cask-initialize)
(require 'pallet)

;; load custom
(add-to-list 'load-path "~/.emacs.d/custom")
(load "my-common-setup.el")
(load "my-auto-complete.el")
(load "my-clojure.el")
(load "my-web.el")
(load "my-haskell.el")
(load "my-scheme.el")
(load "my-org.el")
(load "my-others.el")


