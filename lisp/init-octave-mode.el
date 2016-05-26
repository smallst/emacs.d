;(autoload 'octave-mode "octave-mod" nil t)
 (add-to-list
  'auto-mode-alist
  '("\\.m$" . octave-mode))
(add-hook 'octave-mode-hook
          (lambda ()
            (abbrev-mode 1)
            (auto-fill-mode 1)
            (if (eq window-system 'x)
                (font-lock-mode 1))))
(provide 'init-octave-mode)