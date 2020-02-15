(use-package hydra
  :requires helm
  :config
  ;; Easier cycling of yanking.
  (defhydra yank-pop-hydra ()
    "yank"
    ("C-y" yank nil)
    ("M-y" yank-pop nil)
    ("y" (yank-pop 1) "next")
    ("Y" (yank-pop -1) "prev")
    ("l" helm-show-kill-ring "list" :color blue))

  (global-set-key (kbd "M-y") #'yank-pop-hydra/yank-pop)
  (global-set-key (kbd "C-y") #'yank-pop-hydra/yank)

  (defhydra compilation-hydra (:columns 4)
    "
Command: %(netrom/compilation-command-string)
%(netrom/compilation-scroll-output-string) + %(netrom/compilation-skip-threshold-string)
"
    ("c" compile "Compile")
    ("C" compile-from-buffer-folder "Compile from buffer folder")
    ("r" recompile "Recompile")
    ("k" kill-compilation "Stop")
    ("n" next-error "Next error")
    ("N" next-error-skip-warnings "Next error, skip warnings")
    ("p" previous-error "Previous error")
    ("f" first-error "First error")
    ("l" netrom/compilation-last-error "Last error")
    ("s" netrom/compilation-toggle-scroll "Toggle scroll")
    ("t" netrom/compilation-toggle-threshold "Toggle threshold")
    ("q" nil "Cancel" :color blue))

  (global-set-key [(f5)] 'compilation-hydra/body)

  ;; Define hydra for programming modes.
  (add-hook 'prog-mode-hook
            (lambda ()
              ;; Using local-set-key because defining the bindings in prog-mode-map will get
              ;; overridden by c++-mode bindings, for instance. This shadows them instead.
              (when (member major-mode '(c++-mode c-mode))
                (local-set-key (kbd "C-c C-c") 'compilation-hydra/body)))))
