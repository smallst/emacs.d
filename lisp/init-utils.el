;; -*- coding: utf-8; lexical-binding: t; -*-

(defun local-require (pkg)
  "Require PKG in site-lisp directory."
  (unless (featurep pkg)
    (load (expand-file-name
           (cond
            ((eq pkg 'go-mode-load)
             (format "%s/go-mode/%s" my-site-lisp-dir pkg))
            (t
             (format "%s/%s/%s" my-site-lisp-dir pkg pkg))))
          t t)))

(defun my-ensure (feature)
  "Make sure FEATURE is required."
  (unless (featurep feature)
    (condition-case nil
        (require feature)
      (error nil))))

(defun my-git-root-dir ()
  "Git root directory."
  (locate-dominating-file default-directory ".git"))

(defun my-git-files-in-rev-command (rev level)
  "Return git command line to show files in REV and LEVEL."
  (unless level (setq level 0))
  (concat "git diff-tree --no-commit-id --name-only -r "
          rev
          (make-string level ?^)))

(defun nonempty-lines (s)
  (split-string s "[\r\n]+" t))

(defun my-lines-from-command-output (command)
  "Return lines of COMMAND output."
  (let* ((output (string-trim (shell-command-to-string command)))
         (cands (nonempty-lines output)))
    (delq nil (delete-dups cands))))

(defun run-cmd-and-replace-region (cmd)
  "Run CMD in shell on selected region or whole buffer and replace it with cli output."
  (let* ((orig-point (point))
         (b (if (region-active-p) (region-beginning) (point-min)))
         (e (if (region-active-p) (region-end) (point-max))))
    (shell-command-on-region b e cmd nil t)
    (goto-char orig-point)))

(defun my-use-tags-as-imenu-function-p ()
  "Can use tags file to build imenu function"
  (my-ensure 'counsel-etags)
  (and (locate-dominating-file default-directory "TAGS")
       ;; latest universal ctags has built in parser for javacript/typescript
       (counsel-etags-universal-ctags-p "ctags")
       (memq major-mode '(typescript-mode js-mode javascript-mode))))

;; {{ copied from http://ergoemacs.org/emacs/elisp_read_file_content.html
(defun my-get-string-from-file (file)
  "Return FILE's content."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun my-read-lines (file)
  "Return a list of lines of FILE."
  (split-string (my-get-string-from-file file) "\n" t))
;; }}

(defun my-write-to-file (str file)
  "Write STR to FILE."
  (with-temp-buffer
    (insert str)
    (write-file (file-truename file))))

(defun my-write-to-missing-file (str file)
  "Write STR to FILE if it's missing."
  (unless (file-exists-p file)
    (my-write-to-file str file)))

;; Handier way to add modes to auto-mode-alist
(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

(defun add-interpreter-mode (mode &rest patterns)
  "Add entries to `interpreter-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'interpreter-mode-alist (cons pattern mode))))

(defun my-what-face (&optional position)
  "Shows all faces at POSITION."
  (let* ((face (get-text-property (or position (point)) 'face)))
    (unless (keywordp (car-safe face)) (list face))))

;; String utilities missing from core emacs
(defun string-all-matches (regex str &optional group)
  "Find all matches for `REGEX' within `STR', returning the full match string or group `GROUP'."
  (let ((result nil)
        (pos 0)
        (group (or group 0)))
    (while (string-match regex str pos)
      (push (match-string group str) result)
      (setq pos (match-end group)))
    result))

(defun path-in-directory-p (file directory)
  "FILE is in DIRECTORY."
  (let* ((pattern (concat "^" (file-name-as-directory directory))))
    (if (string-match-p pattern file) file)))

(defun my-prepare-candidate-fit-into-screen (s)
  (let* ((w (frame-width))
         ;; display kill ring item in one line
         (key (replace-regexp-in-string "[ \t]*[\n\r]+[ \t]*" "\\\\n" s)))
    ;; strip the whitespace
    (setq key (replace-regexp-in-string "^[ \t]+" "" key))
    ;; fit to the minibuffer width
    (if (> (length key) w)
        (setq key (concat (substring key 0 (- w 4)) "...")))
    (cons key s)))

(defun my-select-from-kill-ring (fn)
  "If N > 1, yank the Nth item in `kill-ring'.
If N is nil, use `ivy-mode' to browse `kill-ring'."
  (interactive "P")
  (let* ((candidates (cl-remove-if
                       (lambda (s)
                         (or (< (length s) 5)
                             (string-match-p "\\`[\n[:blank:]]+\\'" s)))
                       (delete-dups kill-ring)))
          (ivy-height (/ (frame-height) 2)))
     (ivy-read "Browse `kill-ring':"
               (mapcar #'my-prepare-candidate-fit-into-screen candidates)
               :action fn)))

(defun my-delete-selected-region ()
  "Delete selected region."
  (when (region-active-p)
    (delete-region (region-beginning) (region-end))))

(defun my-insert-str (str)
  "Insert STR into current buffer."
  ;; ivy8 or ivy9
  (if (consp str) (setq str (cdr str)))
  ;; evil-mode?
  (if (and (functionp 'evil-normal-state-p)
           (boundp 'evil-move-cursor-back)
           (evil-normal-state-p)
           (not (eolp))
           (not (eobp)))
      (forward-char))

  (my-delete-selected-region)

  ;; insert now
  (insert str)
  str)

(defun my-line-str (&optional line-end)
  (buffer-substring-no-properties (line-beginning-position)
                                  (if line-end line-end (line-end-position))))

(defun my-is-in-one-line (b e)
  (save-excursion
    (goto-char b)
    (and (<= (line-beginning-position) b)
         (<= e (line-end-position)))))

(defun my-buffer-str ()
  (buffer-substring-no-properties (point-min) (point-max)))

(defun my-selected-str ()
  "Get string of selected region."
  (buffer-substring-no-properties (region-beginning) (region-end)))

(defun my-use-selected-string-or-ask (&optional hint)
  "Use selected region or ask for input.
If HINT is empty, use symbol at point."
  (cond
   ((region-active-p)
    (my-selected-str))
   ((or (not hint) (string= "" hint))
    (thing-at-point 'symbol))
   (t
    (read-string hint))))

(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (if (get-buffer new-name)
        (message "A buffer named '%s' already exists!" new-name)
      (progn
        (rename-file filename new-name 1)
        (rename-buffer new-name)
        (set-visited-file-name new-name)
        (set-buffer-modified-p nil)))))

(defvar load-user-customized-major-mode-hook t)
(defvar cached-normal-file-full-path nil)

(defun buffer-too-big-p ()
  "Test if current buffer is too big."
  ;; 5000 lines
  (> (buffer-size) (* 5000 80)))

(defun my-file-too-big-p (file)
  "Test if FILE is too big."
  (> (nth 7 (file-attributes file))
     (* 5000 64)))

(defvar force-buffer-file-temp-p nil)
(defun is-buffer-file-temp ()
  "If (buffer-file-name) is nil or a temp file or HTML file converted from org file."
  (interactive)
  (let* ((f (buffer-file-name)) (rlt t))
    (cond
     ((not load-user-customized-major-mode-hook)
      (setq rlt t))
     ((not f)
      ;; file does not exist at all
      ;; org-babel edit inline code block need calling hook
      (setq rlt nil))
     ((string= f cached-normal-file-full-path)
      (setq rlt nil))
     ((string-match (concat "^" temporary-file-directory) f)
      ;; file is create from temp directory
      (setq rlt t))
     ((and (string-match "\.html$" f)
           (file-exists-p (replace-regexp-in-string "\.html$" ".org" f)))
      ;; file is a html file exported from org-mode
      (setq rlt t))
     (force-buffer-file-temp-p
      (setq rlt t))
     (t
      (setq cached-normal-file-full-path f)
      (setq rlt nil)))
    rlt))

(defvar my-mplayer-extra-opts ""
  "Extra options for mplayer (ao or vo setup).
For example, you can '(setq my-mplayer-extra-opts \"-ao alsa -vo vdpau\")'.")

(defun my-guess-mplayer-path ()
  "Guess cli program mplayer's path."
  (let* ((rlt "mplayer"))
    (cond
     (*is-a-mac*
      (setq rlt "mplayer -quiet"))

     (*linux*
      (setq rlt (format "mplayer -quiet -stop-xscreensaver %s"
                        my-mplayer-extra-opts)))
     (*cygwin*
      (if (file-executable-p "/cygdrive/c/mplayer/mplayer.exe")
          (setq rlt "/cygdrive/c/mplayer/mplayer.exe -quiet")
        (setq rlt "/cygdrive/d/mplayer/mplayer.exe -quiet")))

     (t ; windows
      (if (file-executable-p "c:\\\\mplayer\\\\mplayer.exe")
          (setq rlt "c:\\\\mplayer\\\\mplayer.exe -quiet")
        (setq rlt "d:\\\\mplayer\\\\mplayer.exe -quiet"))))
    rlt))

(defun my-guess-image-viewer-path (file &optional is-stream)
  (let* ((rlt "mplayer"))
    (cond
     (*is-a-mac*
      (setq rlt
            (format "open %s &" file)))
     (*linux*
      (setq rlt
            (if is-stream (format "curl -L %s | feh -F - &" file) (format "feh -F %s &" file))))
     (*cygwin* (setq rlt "feh -F"))
     (t ; windows
      (setq rlt
            (format "rundll32.exe %s\\\\System32\\\\\shimgvw.dll, ImageView_Fullscreen %s &"
                    (getenv "SystemRoot")
                    file))))
    rlt))

(defun my-gclip ()
  "Get clipboard content."
  (let* ((powershell-program (executable-find "powershell.exe")))
    (cond
     ;; Windows
     ((fboundp 'w32-get-clipboard-data)
      ;; `w32-set-clipboard-data' makes `w32-get-clipboard-data' always return null
      (w32-get-clipboard-data))

     ;; Windows 10
     (powershell-program
      (string-trim-right
       (with-output-to-string
         (with-current-buffer standard-output
           (call-process powershell-program nil t nil "-command" "Get-Clipboard")))))

     ;; xclip can handle
     (t
      (xclip-get-selection 'clipboard)))))

(defvar my-ssh-client-user nil
  "User name of ssh client.")

(defun my-pclip (str-val)
  "Put STR-VAL into clipboard."
  (let* ((win64-clip-program (executable-find "clip.exe"))
         ssh-client)
    (cond
     ;; Windows
     ((fboundp 'w32-set-clipboard-data)
      (w32-set-clipboard-data str-val))

     ;; Windows 10
     ((and win64-clip-program)
      (with-temp-buffer
        (insert str-val)
        (call-process-region (point-min) (point-max) win64-clip-program)))

     ;; If Emacs is inside an ssh session, place the clipboard content
     ;; into "~/.tmp-clipboard" and send it back into ssh client
     ;; Make sure you already set up ssh correctly.
     ;; Only enabled if ssh server is macOS
     ((and (setq ssh-client (getenv "SSH_CLIENT"))
           (not (string= ssh-client ""))
           *is-a-mac*)
      (let* ((file "~/.tmp-clipboard")
             (ip (car (split-string ssh-client "[ \t]+")))
             (cmd (format "scp %s %s@%s:~/" file my-ssh-client-user ip)))
        (when my-ssh-client-user
          (my-write-to-file str-val file)
          (shell-command cmd)
          ;; clean up
          (delete-file file))))

     ;; xclip can handle
     (t
      (xclip-set-selection 'clipboard str-val)))))
;; }}

(defun should-use-minimum-resource ()
  "Some files should use minimum resource (no syntax highlight, no line number display)."
  (and buffer-file-name
       (string-match-p "\.\\(mock\\|min\\|bundle\\)\.js" buffer-file-name)))

(defun my-async-shell-command (command)
  "Execute string COMMAND asynchronously."
  (let* ((proc (start-process "Shell"
                              nil
                              shell-file-name
                              shell-command-switch command)))
    (set-process-sentinel proc `(lambda (process signal)
                                  (let* ((status (process-status process)))
                                    (when (memq status '(exit signal))
                                      (unless (string= (substring signal 0 -1) "finished")
                                        (message "Failed to run \"%s\"." ,command))))))))

;; reply y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)
;; {{ code is copied from https://liu233w.github.io/2016/09/29/org-python-windows.org/

(defun my-setup-language-and-encode (language-name coding-system)
  "Set up LANGUAGE-NAME and CODING-SYSTEM at Windows.
For example,
- \"English\" and 'utf-16-le
- \"Chinese-GBK\" and 'gbk"
  (cond
   ((eq system-type 'windows-nt)
    (set-language-environment language-name)
    (prefer-coding-system 'utf-8)
    (set-terminal-coding-system coding-system)

    (modify-coding-system-alist 'process "*" coding-system)
    (defun my-windows-shell-mode-coding ()
      (set-buffer-file-coding-system coding-system)
      (set-buffer-process-coding-system coding-system coding-system))
    (add-hook 'shell-mode-hook #'my-windows-shell-mode-coding)
    (add-hook 'inferior-python-mode-hook #'my-windows-shell-mode-coding)

    (defun my-org-babel-execute:python-hack (orig-func &rest args)
      ;; @see https://github.com/Liu233w/.spacemacs.d/issues/6
      (let* ((coding-system-for-write 'utf-8))
        (apply orig-func args)))
    (advice-add 'org-babel-execute:python :around #'my-org-babel-execute:python-hack))

   (t
    (set-language-environment "UTF-8")
    (prefer-coding-system 'utf-8))))
;; }}

(defun my-skip-white-space (start step)
  "Skip white spaces from START, return position of first non-space character.
If STEP is 1,  search in forward direction, or else in backward direction."
  (let* ((b start)
         (e (if (> step 0) (line-end-position) (line-beginning-position))))
    (save-excursion
      (goto-char b)
      (while (and (not (eq b e)) (memq (following-char) '(9 32)))
        (forward-char step))
      (point))))

(defun my-comint-current-input-region ()
  "Region of current shell input."
  (cons (process-mark (get-buffer-process (current-buffer)))
        (line-end-position)))

(defun my-comint-kill-current-input ()
  "Kill current input in shell."
  (interactive)
  (let* ((region (my-comint-current-input-region)))
    (kill-region (car region) (cdr region))))

(defun my-comint-current-input ()
  "Get current input in shell."
  (let* ((region (my-comint-current-input-region)))
    (string-trim (buffer-substring-no-properties (car region) (cdr region)))))

(defun my-rescan-imenu-items (&optional index-function)
  "Get imenu items using INDEX-FUNCTION."
  (my-ensure 'imenu)
  (let* ((imenu-auto-rescan t)
         (imenu-create-index-function (or index-function imenu-create-index-function))
         (imenu-auto-rescan-maxout (buffer-size))
         (items (imenu--make-index-alist t)))
    (delete (assoc "*Rescan*" items) items)))

(defun my-create-range (&optional inclusive)
  "Return range by font face.
Copied from 3rd party package evil-textobj."
  (let* ((point-face (my-what-face))
         (pos (point))
         (backward-point pos) ; last char when stop, including white space
         (backward-none-space-point pos) ; last none white space char
         (forward-point pos) ; last char when stop, including white space
         (forward-none-space-point pos) ; last none white space char
         (start pos)
         (end pos))

    ;; check chars backward,
    ;; stop when char is not white space and has different face
    (save-excursion
      (let ((continue t))
        (while (and continue (>= (- (point) 1) (point-min)))
          (backward-char)
          (if (= 32 (char-after))
              (setq backward-point (point))
            (if (equal point-face (my-what-face))
                (progn (setq backward-point (point))
                       (setq backward-none-space-point (point)))
              (setq continue nil))))))

    ;; check chars forward,
    ;; stop when char is not white space and has different face
    (save-excursion
      (let ((continue t))
        (while (and continue (< (+ (point) 1) (point-max)))
          (forward-char)
          (let ((forward-point-face (my-what-face)))
            (if (= 32 (char-after))
                (setq forward-point (point))
              (if (equal point-face forward-point-face)
                  (progn (setq forward-point (point))
                         (setq forward-none-space-point (point)))
                (setq continue nil)))))))

    (cond
     (inclusive
      (setq start backward-none-space-point)
      (setq end forward-none-space-point))
     (t
      (setq start (1+ backward-none-space-point))
      (setq end (1- forward-none-space-point))))

    (cons start (1+ end))))

(defun my-pinyinlib-build-regexp-string (str)
  "Build pinyin regexp from STR."
  (my-ensure 'pinyinlib)
  (let* (rlt (i 0) ch)
    (while (< i (length str))
      (setq ch (elt str i))
      (setq rlt (concat rlt
                        (cond
                         ((and (<= ?a ch) (<= ch ?z))
                          (pinyinlib-build-regexp-char ch))
                         (t
                          (char-to-string ch)))))
      (setq i (1+ i)))
    rlt))

(defvar my-disable-idle-timer nil
  "Function passed to `my-run-with-idle-timer' is run immediately.")

(defun my-run-with-idle-timer (seconds func)
  "After SECONDS, run function FUNC once."
  (cond
   (my-disable-idle-timer
    (funcall func))
   (t
    (run-with-idle-timer seconds nil func))))

(defun my-get-closest-imenu-item (cands)
  "Return closest imen item from CANDS."
  (let* ((pos (point))
         closest)
    (dolist (c cands)
      (let* ((item (cdr c))
             (m (cdr item)))
        (when (and m (<= (marker-position m) pos))
          (cond
           ((not closest)
            (setq closest item))
           ((< (- pos (marker-position m))
               (- pos (marker-position (cdr closest))))
            (setq closest item))))))
    closest))

(provide 'init-utils)
;;; init-utils.el ends here
