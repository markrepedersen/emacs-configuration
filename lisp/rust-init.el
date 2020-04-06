(use-package cargo
  :defer t
  :config
  (setq cargo-process--enable-rust-backtrace t)
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :defer t
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package rust-mode
  :defer t
  :mode-hydra
  (rust-mode
   (:color pink :title "Rust mode")
   ("Build"
    (("b" cargo-process-build "Build" :exit)
     ("r" cargo-process-run "Run" :exit)
     ("R" cargo-process-run-bin "Run (specific)")
     ("t" cargo-process-test "Test")
     ("c" cargo-process-clean "Clean"))

    "Docs"
    (("d" cargo-process-doc "Doc")
     ("D" cargo-process-doc-open "Doc (open)")
     ("u" cargo-process-update "Update")
     ("C" cargo-process-check "Check")
     ("a" cargo-process-audit "Audit")
     ("C-c" cargo-process-clippy "Clippy"))

    "Errors"
    (("n" next-error "Next" :exit nil)
     ("N" next-error-skip-warnings "Next, skip warnings" :exit nil)
     ("p" previous-error "Previous" :exit nil)
     ("f" first-error "First")
     ("k" kill-compilation "Stop"))))
  :config
  (setq-default xref-prompt-for-identifier nil))

;; Show xref results in helm.
(use-package helm-xref
  :defer t
  :requires helm
  :preface
  (defadvice dired-do-find-regexp-and-replace
      (around netrom-no-helm-dired-do-find-regexp-and-replace activate)
    (let ((xref-show-xrefs-function 'xref--show-xref-buffer))
      ad-do-it))
  (defun netrom/xref-find-apropos-at-point (pattern)
    "Xref find apropos at point, if anything, and show prompt for PATTERN."
    (interactive
     (list
      (read-string "Xref find apropos of: " (thing-at-point 'symbol))))
    (xref-find-apropos pattern))
  :config
  (setq-default xref-show-xrefs-function 'helm-xref-show-xrefs)
  (setq helm-xref-candidate-formatting-function 'helm-xref-format-candidate-long))
