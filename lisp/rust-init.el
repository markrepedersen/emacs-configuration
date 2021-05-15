(use-package rustic
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
     ("k" kill-compilation "Stop")))))
