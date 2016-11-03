(pdf-tools-install)
(define-pdf-cache-function pagelabels)
(defun pdf-view-page-number ()
  (interactive)
   (message " [%s/%s/%s]"
            (nth (1- (pdf-view-current-page))
                (pdf-cache-pagelabels))
            (number-to-string (pdf-view-current-page))
            (number-to-string (pdf-cache-number-of-pages))))
(provide 'init-pdf)