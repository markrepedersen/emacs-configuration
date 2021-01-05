(use-package rustic
  :hook (cargo-minor-mode)
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
  :init
  (setq auto-mode-alist (delete '("\\.rs\\'" . rust-mode) auto-mode-alist))
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rustic-mode))
  :config
  (setq-default xref-prompt-for-identifier nil))

(use-package cargo
  :after rustic
  :defer t)
