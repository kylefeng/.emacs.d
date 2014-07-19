;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Package

;; Marmalade
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages 
  '(starter-kit
    starter-kit-lisp
    starter-kit-bindings
    starter-kit-eshell
    clojure-mode
    clojure-test-mode
    cider
    ac-nrepl
    coffee-mode
    erlang
    auto-complete
    markdown-mode
    highlight-parentheses
    yaml-mode
    vline
    hl-line+
    col-highlight
    crosshairs
    haskell-mode
    soft-morning-theme
    json-mode)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

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


