(defun c-wx-lineup-topmost-intro-cont (langelem)
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward "EVT_" (line-end-position) t)
      'c-basic-offset
      (c-lineup-topmost-intro-cont langelem))))

;; avoid default "gnu" style, use more popular one
(setq c-default-style "linux")

(defun fix-c-indent-offset-according-to-syntax-context (key val)
  ;; remove the old element
  (setq c-offsets-alist (delq (assoc key c-offsets-alist) c-offsets-alist))
  ;; new value
  (add-to-list 'c-offsets-alist '(key . val)))

(defun my-common-cc-mode-setup ()
  "setup shared by all languages (java/groovy/c++ ...)"
  (setq c-basic-offset 4)
  ;; give me NO newline automatically after electric expressions are entered
  (setq c-auto-newline t)

  ;; syntax-highlight aggressively
  ;; (setq font-lock-support-mode 'lazy-lock-mode)
  (setq lazy-lock-defer-contextually t)
  (setq lazy-lock-defer-time 0)

  ;make DEL take all previous whitespace with it
  (c-toggle-hungry-state 1)

  ;; indent
  ;; google "C/C++/Java code indentation in Emacs" for more advanced skills
  ;; C code:
  ;;   if(1) // press ENTER here, zero means no indentation
  (fix-c-indent-offset-according-to-syntax-context 'substatement 0)
  ;;   void fn() // press ENTER here, zero means no indentation
  (fix-c-indent-offset-according-to-syntax-context 'func-decl-cont 0))

(defun my-c-mode-setup ()
  "C/C++ only setup"
  (message "my-c-mode-setup called (buffer-file-name)=%s" (buffer-file-name))
  ;; @see http://stackoverflow.com/questions/3509919/ \
  ;; emacs-c-opening-corresponding-header-file
  (local-set-key (kbd "C-x C-o") 'ff-find-other-file)

  (setq cc-search-directories '("." "/usr/include" "/usr/local/include/*" "../*/include" "$WXWIN/include"))

  ;; wxWidgets setup
  (c-set-offset 'topmost-intro-cont 'c-wx-lineup-topmost-intro-cont)

  ;; make a #define be left-aligned
  (setq c-electric-pound-behavior (quote (alignleft)))

  (when buffer-file-name

    ;; @see https://github.com/redguardtoo/cpputils-cmake
    ;; Make sure your project use cmake!
    ;; Or else, you need comment out below code:
    ;; {{
    (flymake-mode 1)
    (if (executable-find "cmake")
        (if (not (or (string-match "^/usr/local/include/.*" buffer-file-name)
                     (string-match "^/usr/src/linux/include/.*" buffer-file-name)))
            (cppcm-reload-all)))
    ;; }}

    ))

;; donot use c-mode-common-hook or cc-mode-hook because many major-modes use this hook
(defun c-mode-common-hook-setup ()
  (unless (is-buffer-file-temp)
    (my-common-cc-mode-setup)
    (unless (or (derived-mode-p 'java-mode) (derived-mode-p 'groovy-mode))
      (my-c-mode-setup))

    ;; gtags (GNU global) stuff
    (when (and (executable-find "global")
               ;; `man global' to figure out why
               (not (string-match-p "GTAGS not found"
                                    (shell-command-to-string "global -p"))))
      ;; emacs 24.4+ will set up eldoc automatically.
      ;; so below code is NOT needed.
      (eldoc-mode 1))
 (defun myself-cc-mode-hook ()
  (set (make-local-variable 'my-exec-command)
       (let ((myfile (if cppcm-build-dir
                         (cppcm-get-exe-path-current-buffer)
                       (file-name-sans-extension buffer-file-name))))
         (format "%s;echo Press any key to continue;read -n"
                 myfile)))
  (set (make-local-variable 'my-gdb-command)
       (let ((myfile (if cppcm-build-dir
                         (cppcm-get-exe-path-current-buffer)
                       (file-name-sans-extension buffer-file-name))))
         (format "gdb -i=mi %s"
                 myfile)))
  (global-set-key [f9] 'myself-compile)
  (global-set-key [f8] 'myself-exec)
  (global-set-key (kbd "S-<f8>") 'myself-gdb))
(defun myself-c-mode-hook ()
  (if cppcm-src-dir
      (set (make-local-variable 'my-compile-command)
           compile-command)
    (set (make-local-variable 'my-compile-command)
         ;; emulate make's .c.o implicit pattern rule, but with
         ;; different defaults for the CC, CPPFLAGS, and CFLAGS
         ;; variables:
         ;; $(CC) -c -o $@ $(CPPFLAGS) $(CFLAGS) $<
         (let ((myfile (file-name-nondirectory buffer-file-name)))
           (format "%s -o %s %s %s" 
                   (or (getenv "CC") "gcc")
                   (file-name-sans-extension myfile)
                   ;; (or (getenv "CPPFLAGS") "-DDEBUG=9")
                   (or (getenv "CFLAGS") "-Wall -g")
                   myfile))))
  ;; (set (make-local-variable 'my-exec-command)
  ;;      (let ((myfile (file-name-sans-extension buffer-file-name)))
  ;;        (format "%s;echo Press any key to continue;read -n"
  ;;                myfile)))
  )
(defun myself-c++-mode-hook ()
  (if cppcm-src-dir
      (set (make-local-variable 'my-compile-command)
           compile-command)
    (set (make-local-variable 'my-compile-command)
         (let ((myfile (file-name-nondirectory buffer-file-name)))
           (format "%s -o %s %s %s"
                   (or (getenv "CC") "g++")
                   (file-name-sans-extension myfile)
                   (or (getenv "CFLAGS") "-Wall -g -std=c++11")
                   myfile))) )
  
  ;; (set (make-local-variable 'my-exec-command)
  ;;      (let ((myfile (file-name-sans-extension buffer-file-name)))
  ;;        (format "%s;echo Press any key to continue;read -n"
  ;;                myfile)))
  )
(defun myself-exec ()
  (interactive)
  (progn
    (let* ((buf-name (generate-new-buffer-name "result"))
           (buf (get-buffer-create buf-name)))
      (async-shell-command my-exec-command buf)
      (other-window 1)
      (let ((proc (get-buffer-process buf)))
        (if (and proc (null (eq (process-status proc) 'exit)))
            (set-process-sentinel proc '(lambda (proc event)
                                          (if (eq (process-status proc) 'exit)
                                              (with-current-buffer (process-buffer proc) (dying-mode 't)))))
          (delete-window)))))
  )
(defun myself-compile ()
  (interactive)
  (save-buffer)
  (compile my-compile-command))
(defun myself-gdb ()
  (interactive)
  (gdb my-gdb-command))

(add-hook 'c-mode-hook
          (lambda ()
            (when (buffer-file-name)
              
              ( myself-c-mode-hook)
              (myself-cc-mode-hook))))
(add-hook 'c++-mode-hook
          (lambda ()
            (when (buffer-file-name)
              (myself-cc-mode-hook)
              (myself-c++-mode-hook)
              )))
   ))
(add-hook 'c-mode-common-hook 'c-mode-common-hook-setup)

(provide 'init-cc-mode)
