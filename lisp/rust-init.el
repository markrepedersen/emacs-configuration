(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package rust-mode
  :demand
  :config
  (defun netrom/toggle-rust-backtrace ()
    "Toggle between Rust backtrace enabled and disabled."
    (interactive)
    (if (not cargo-process--enable-rust-backtrace)
        (setq cargo-process--enable-rust-backtrace t)
      (setq cargo-process--enable-rust-backtrace nil)))

  (defun netrom/rust-backtrace-string ()
    (interactive)
    (if (not cargo-process--enable-rust-backtrace)
        "Backtrace: disabled"
      "Backtrace: enabled"))

  (defhydra hydra-rust (:exit t :hint nil)
    ("b" cargo-process-build "Build" :column "Cargo")
    ("r" cargo-process-run "Run")
    ("R" cargo-process-run-bin "Run (specific)")
    ("t" cargo-process-test "Test")
    ("c" cargo-process-clean "Clean")

    ("d" cargo-process-doc "Doc" :column "")
    ("D" cargo-process-doc-open "Doc (open)")
    ("u" cargo-process-update "Update")
    ("C" cargo-process-check "Check")
    ("a" cargo-process-audit "Audit")
    ("C-c" cargo-process-clippy "Clippy")

    ("n" next-error "Next" :column "Errors")
    ("N" next-error-skip-warnings "Next, skip warnings")
    ("p" previous-error "Previous")
    ("f" first-error "First")
    ("l" netrom/compilation-last-error "Last")
    ("k" kill-compilation "Stop")

    ("C-b" netrom/toggle-rust-backtrace "Toggle backtrace" :column "Misc")
    ("q" nil "Cancel" :color blue))

  (global-set-key (kbd "C-c r") 'hydra-rust/body))

;; Don't show prompt unless nothing is under point or if it has to show it.
(setq-default xref-prompt-for-identifier nil)

;; Show xref results in helm.
(use-package helm-xref
  :requires helm
  :config
  ;; Use helm-xref as the default xref show function.
  (setq-default xref-show-xrefs-function 'helm-xref-show-xrefs)

  ;; Show full filename in results instead of only basename which doesn't give enough context.
  (setq helm-xref-candidate-formatting-function 'helm-xref-format-candidate-long)

  ;; Setting `helm-xref-show-xrefs' as the xref show function breaks `find-name-dired' interactive
  ;; search-and-replace across files, which uses `dired-do-find-regexp-and-replace'. Thus we make
  ;; xref show results in the default way by setting `xref-show-xrefs-function' to
  ;; `xref--show-xref-buffer' via an around-advice.
  (defadvice dired-do-find-regexp-and-replace
      (around netrom-no-helm-dired-do-find-regexp-and-replace activate)
    (let ((xref-show-xrefs-function 'xref--show-xref-buffer))
      ad-do-it)))

(defun netrom/xref-find-apropos-at-point (pattern)
  "Xref find apropos at point, if anything, and show prompt for PATTERN."
  (interactive
   (list
    (read-string "Xref find apropos of: " (thing-at-point 'symbol))))
  (xref-find-apropos pattern))
