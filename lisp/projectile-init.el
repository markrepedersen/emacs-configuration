(use-package projectile
  :after hydra
  :hydra (hydra-projectile (:hint nil :exit t)
			   "Projectile"
			   ("a" helm-projectile "all" :column "Show all")
			   ("f" helm-projectile-find-file "find file" :column "Navigation")
			   ("p" helm-projectile-switch-project "switch project")
			   ("b" projectile-save-project-buffers "save buffers" :column "Buffers"))
  :bind (("C-c p" . hydra-projectile/body))
  :config
  (setq projectile-enable-caching t
        projectile-sort-order 'recently-active
	projectile-file-exists-remote-cache-expire nil)
  (projectile-mode))

(use-package helm-projectile
  :after (projectile helm)
  :config
  (helm-projectile-on))
