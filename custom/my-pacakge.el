;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Package
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

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

(defun windows-platform-pacakge ()
  (require 'package)
  (package-initialize)
  
  (when (not package-archive-contents)
    (package-refresh-contents))
  
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(defun other-platform-package ()
  (require 'cask "/usr/local/Cellar/cask/0.7.0/cask.el") ;; Cask
  (cask-initialize)
  (require 'pallet))

(if (eq system-type 'windows-nt)
    (windows-platform-pacakge)
  (other-platform-package))




