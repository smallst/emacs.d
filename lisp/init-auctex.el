(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
(defun my-zathura-hook ()
  (add-to-list 'TeX-view-program-selection
               '(output-pdf "Zathura"))
  (TeX-source-correlate-mode 1)
)
(add-hook 'TeX-mode-hook 'my-zathura-hook)
(provide 'init-auctex)