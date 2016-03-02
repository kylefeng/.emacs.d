(require 'go-mode)

(setenv "GOPATH" "/Users/kylefeng/development/my-projects/go")
(add-to-list 'exec-path "/Users/kylefeng/development/my-projects/go/bin")

(defun my-go-mode-hook ()
  ; Use goimports instead of gofmt
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)

  ; go-oracle
  (load "$GOPATH/src/golang.org/x/tools/cmd/oracle/oracle.el") 

  ; key bindings
  (local-set-key (kbd "C-c C-r")
                 'go-remove-unused-imports)
  (local-set-key (kbd "C-c C-g")
                 'go-goto-imports)
  (local-set-key (kbd "C-c C-f")
                 'gofmt)
  (local-set-key (kbd "C-c C-k")
                 'godoc)

  (set (make-local-variable 'company-backends) '(company-go))
  (company-mode))

(add-hook 'go-mode-hook 'my-go-mode-hook)


