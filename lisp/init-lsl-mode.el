(require 'xlsl-mode)
(add-to-list 'auto-mode-alist '("\\.lsl\\'" . xlsl-mode))
(setq xlsl-xlslint-path "~/Public/lslint/lslint")
(defun my-xlsl-mode-customizations ()
  "Some customization for xlsl-mode."
  (define-key xlsl-mode-map (kbd "C-l") 'xlsl-complete-symbol)
)

(add-hook 'xlsl-mode-hook 'my-xlsl-mode-customizations)

(provide 'init-lsl-mode)