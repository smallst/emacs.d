(add-hook 'go-mode-hook (lambda ()
                          (add-to-list 'company-backends 'company-go)))
(add-hook 'go-mode-hook 'go-eldoc-setup)
(add-to-list 'load-path "~/go/src/github.com/dougm/goflymake")
(eval-after-load "go-mode"
  '(progn 
     (require 'go-flymake)))

(provide 'init-go-mode)
