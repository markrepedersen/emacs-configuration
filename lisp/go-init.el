(use-package go-mode
  :functions (go-packages-gopkgs go-update-tools)
  :bind (:map go-mode-map
         ([remap xref-find-definitions] . godef-jump)
         ("C-c R" . go-remove-unused-imports)
         ("<f1>" . godoc-at-point))
  :config
  ;; Env vars
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-envs '("GOPATH" "GO111MODULE" "GOPROXY"))))


;; Local Golang playground for short snippets
(use-package go-playground
  :diminish)
