(use-package groovy-mode
  :defer t
  :ensure t
  :config
  (setq lsp-groovy-server-file (expand-file-name "groovy-language-server/groovy-language-server-all.jar" user-emacs-directory))
  :mode
  (("Jenkinsfile\\'" . groovy-mode))
  ("gradle" . groovy-mode))
