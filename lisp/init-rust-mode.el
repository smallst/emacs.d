(setq racer-cmd "~/.cargo/bin/racer") ;; Rustup binaries PATH
(setq racer-rust-src-path "/home/smallst/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src") ;; Rust source code PATH

(require 'flymake-rust)
(setq flymake-rust-use-cargo 1)
(add-hook 'rust-mode-hook 'flymake-rust-load)
(defun my-setup-rust ()
  (interactive)
  (unless (is-buffer-file-temp)
    (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
    ))
(add-hook 'rust-mode-hook 'cargo-minor-mode)
(add-hook 'rust-mode-hook 'racer-mode)
;; (add-hook 'racer-mode-hook 'eldoc-mode)
(add-hook 'racer-mode-hook 'company-mode)
(add-hook 'rust-mode-hook 'my-setup-rust)

;; (setq company-tooltip-align-annotations t)
(provide 'init-rust-mode)