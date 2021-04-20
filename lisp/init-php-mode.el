(require 'flymake-php)
(defun my-php-hook ()
  (hs-minor-mode 1))

(add-hook 'web-mode-hook 'my-php-hook)

(provide 'init-php-mode)