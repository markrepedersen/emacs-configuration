(use-package yasnippet
  :defer t
  :pretty-hydra
  ((:color teal :quit-key "q" :title (with-mode-icon 'yasnippet "Snippets"))
   ("Snippets"
    (("a" company-yasnippet               "View All")
     ("n" yas-new-snippet                 "New")
     ("t" yas-tryout-snippet              "Tryout"))))
  :bind (("C-c y" . yasnippet-hydra/body))
  :config
  (yas-global-mode t)
  (add-to-list #'yas-snippet-dirs (expand-file-name "snippets" user-emacs-directory))
  (yas-reload-all)
  (setq yas-prompt-functions '(yas-ido-prompt))
  :diminish yas-minor-mode)

(use-package yasnippet-snippets
  :defer t
  :after yasnippet)
