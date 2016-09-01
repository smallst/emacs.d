(add-hook 'go-mode-hook (lambda ()
                          (add-to-list 'company-backends 'company-go)))
(add-hook 'go-mode-hook 'go-eldoc-setup)
(eval-after-load "go-mode"
  '(require 'flymake-go))

(provide 'init-go-mode)