(use-package go-mode
  :functions (go-packages-gopkgs go-update-tools)
  :mode ("\\.go\\'" . go-mode)
  :mode-hydra
  ("Doc"
   (("d" godoc-at-point "doc at point"))
   "Imports"
   (("ia" go-import-add "add")
    ("ir" go-remove-unused-imports "cleanup"))))
