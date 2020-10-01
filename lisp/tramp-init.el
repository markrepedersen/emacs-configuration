(use-package helm-tramp
  :after (helm tramp)
  :pretty-hydra
  ((:color teal :quit-key "q" :title (with-mode-icon 'lsp-mode "LSP mode"))
   ("Tramp"
    (("s" helm-tramp "SSH")
     ("q" helm-tramp-quit "Quit"))))
  :bind (("C-c s" . helm-tramp-hydra/body))
  :config
  (setq make-backup-files nil
	create-lockfiles nil)
  (add-to-list 'helm-commands-using-frame 'helm-tramp)
  (add-hook 'helm-tramp-pre-command-hook '(lambda ()
					    (projectile-mode 0)))
  (add-hook 'helm-tramp-quit-hook '(lambda ()
				     (projectile-mode 1))))
