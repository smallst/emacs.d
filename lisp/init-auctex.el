(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
(defun my-zathura-hook ()
  (add-to-list 'TeX-view-program-selection
               ;'(output-pdf "Zathura")
               '(output-pdf "PDF Tools")
               )
  )
  
(setq  TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
       TeX-source-correlate-start-server t
       TeX-source-correlate-mode 1)
       
;(add-hook 'LaTeX-mode-hook (lambda()
;                             (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
;                             (setq TeX-command-default "XeLaTeX")
;))
(setq-default TeX-engine 'xetex)
(add-hook 'TeX-after-compilation-finished-functions
        #'TeX-revert-document-buffer)
(add-hook 'TeX-mode-hook 'my-zathura-hook)
(add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)
(provide 'init-auctex)