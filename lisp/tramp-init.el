(use-package tramp
  :defer t
  :config
  (setq tramp-default-method "ssh")
  (tramp-change-syntax 'simplified))
