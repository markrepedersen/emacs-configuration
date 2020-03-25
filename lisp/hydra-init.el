(use-package hydra
  :requires helm
  :bind (("C-c q" . hydra-utils/body))
  :hydra (hydra-utils (:exit t :hint nil :columns 1)
  "Utility functions"
  ("i" (lambda () (interactive) (load-file user-init-file)) "Load init file")))
