(use-package go-mode
  :functions (go-packages-gopkgs go-update-tools)
  :mode-hydra
  ("Doc"
   (("d" godoc-at-point "doc at point"))
   "Imports"
   (("ia" go-import-add "add")
    ("ir" go-remove-unused-imports "cleanup")))
  :bind (:map go-mode-map
              ([remap xref-find-definitions] . godef-jump)
              ("C-c R" . go-remove-unused-imports)
              ("<f1>" . godoc-at-point))
  :config
  ;; Env vars
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-envs '("GOPATH" "GO111MODULE" "GOPROXY"))))
