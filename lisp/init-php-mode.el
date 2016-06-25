
(defun my-php-hook ()
  (ggtags-mode 1)
  (hs-minor-mode 1))

(add-hook 'php-mode-hook 'my-php-hook)

(provide 'init-php-mode)