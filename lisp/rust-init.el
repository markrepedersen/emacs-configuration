(use-package flycheck-rust
  :defer t)

(use-package cargo
  :defer t)

(use-package rustic
  :demand
  :hook ((cargo-minor-mode)
	 (flycheck-rust-setup))
  :mode-hydra
  (rustic-mode
   (:title (with-mode-icon 'rust-mode "Rust"))
   ("Build"
    (("b" rustic-cargo-build "Build" :exit)
     ("r" rustic-cargo-run "Run" :exit)
     ("t" rustic-cargo-test "Test")
     ("T" rustic-cargo-current-test "Test (current)")
     ("B" (lambda () (setq rustic-compile-backtrace t) "Backtrace")))

    "Init"
    (("C" rustic-cargo-new "New")
     ("I" cargo-process-init "New"))

    "Errors"
    (("n" next-error "Next" :exit nil)
     ("p" previous-error "Previous" :exit nil)
     ("f" first-error "First")
     ("k" kill-compilation "Stop"))))
  :config
  (setq-default xref-prompt-for-identifier nil)
  (setq rustic-lsp-server 'rust-analyzer))

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
