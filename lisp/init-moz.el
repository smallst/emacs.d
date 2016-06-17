(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)

;; {{ mozrepl auto-refresh browser
(defun moz-reload-browser ()
  (interactive)
  (let (js-cond cmd)
    (if (fboundp 'my-moz-refresh-browser-condition)
        (setq js-cond (funcall 'my-moz-refresh-browser-condition (buffer-file-name))))
    (cond
     (js-cond
      (setq cmd (concat "if(" js-cond "){setTimeout(function(){content.document.location.reload(true);}, '500');}")))
     (t
      (setq cmd "setTimeout(function(){content.document.location.reload(true);}, '500');")))
    (comint-send-string (inferior-moz-process) (concat "repl.switch_tab(refreshtab);" cmd))))
;; added by me 
(defun moz-controller-repl-filter (proc string)
  "Filter function of *MozRepl*.
It gets the useful output of *MozRepl*, store it in `moz-controller-repl-output` and `kill-ring`"
  (when (buffer-live-p (process-buffer proc))
    (unless (string= string "repl> ") ; ignore empty output (page up, page down, etc)
      (setq moz-controller-repl-output 
            (replace-regexp-in-string "\\(.+\\)\n\nrepl> " "\\1" string))
                                        ;(kill-new moz-controller-repl-output) ; append to kill-ring
                                        ;(message moz-controller-repl-output) ; show the copied content in echo area
                                        )
    
    ;; (with-current-buffer (process-buffer proc)
    ;;   (let ((moving (= (point) (process-mark proc))))
    ;;     (save-excursion
    ;;       ;; Insert the text, advancing the process marker.
    ;;       (goto-char (process-mark proc))
    ;;       (insert string)
    ;;       (set-marker (process-mark proc) (point)))
    ;;     (if moving (goto-char (process-mark proc)))))
    ))


(defvar moz-reload-browser-when-save t 
  "Reload the browser when save")

(defun moz-after-save ()
  (interactive)
  (if (memq major-mode '(web-mode html-mode nxml-mode nxhml-mode php-mode css-mode js2-mode))
      (if moz-reload-browser-when-save
          (moz-reload-browser))))
;; }}
(defun my-ivy-list (retstring)
  "firefox pages"
  (interactive)
  (let (collection)
    (unless recentf-mode (recentf-mode 1))
    (setq collection (split-string retstring "\n" t))
    (ivy-completing-read "pages:" collection :action (lambda (x) 
                                        (comint-send-string (inferior-moz-process) (concat "refreshtab=repl.tabs[" (number-to-string (cl-position x (split-string retstring "\n" t) :test 'equal)) "];"))))))
(defun my-moz-setup ()
  (interactive)
  (comint-send-string (inferior-moz-process) "var refreshtab;")
  (set-process-filter (get-buffer-process "*MozRepl*") 'moz-controller-repl-filter)
  (comint-send-string (inferior-moz-process) "repl.show_tab();")
  (sleep-for 0.1)
  (my-ivy-list moz-controller-repl-output))

(defun moz-custom-setup ()
  ;; called when editing a REAL file
  (unless (is-buffer-file-temp)
    (moz-minor-mode 1)
    (setq moz-quiet t)
    ;; @see  http://www.emacswiki.org/emacs/MozRepl
    ;; Example - you may want to add hooks for your own modes.
    ;; I also add this to python-mode when doing django development.
    (add-hook 'after-save-hook
              'moz-after-save
              'append 'local)))

;; (add-hook 'js2-mode-hook 'moz-custom-setup)
(add-hook 'html-mode-hook 'moz-custom-setup)
(add-hook 'nxml-mode-hook 'moz-custom-setup)
(add-hook 'web-mode-hook 'moz-custom-setup)
(add-hook 'js2-mode-hook 'moz-custom-setup)
(add-hook 'js-mode-hook 'moz-custom-setup)
(add-hook 'css-mode-hook 'moz-custom-setup)

(defun moz-goto-content-and-run-cmd (cmd)
  (message "moz-goto-content-and-run-cmd called => %s" cmd)
  ;; repl.enter() is NOT needed any more
  ;; just use content.document to acess firefox page directly
  (comint-send-string (inferior-moz-process) cmd))

(setq moz-repl-js-dir (expand-file-name "~/moz-repl-js-dir"))

(defun moz--read-file (js-file)
  (with-temp-buffer
    (insert-file-contents js-file)
    (buffer-string)))

(defun moz--load-js-file (js-file)
  (let (cmd)
    (message "moz--load-js-file called => %s" js-file)
    (when (file-exists-p js-file)
      (message "file does exist")
      ;; make moz API usable in any major-mode
      (moz-minor-mode 1)
      ;; flush mozrepl at first
      ;; read the content of js-file
      (setq cmd (moz--read-file js-file))
      (moz-goto-content-and-run-cmd cmd))))

(defun moz-load-js-file-and-send-it ()
  "load js file from specific directory and send it to mozrepl"
  (interactive)
  (let ((js-file (read-file-name "js file:" moz-repl-js-dir)))
    (moz--load-js-file js-file)))

(defun moz-console-clear ()
  (interactive)
  (moz-minor-mode 1)
  (moz-goto-content-and-run-cmd "content.console.log('clearing ...');")
  (moz-goto-content-and-run-cmd "content.console.clear();"))

(provide 'init-moz)
