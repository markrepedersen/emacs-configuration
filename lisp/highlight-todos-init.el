;; Marks TODO, FIXME etc. clearly.
(use-package fic-mode
  :config
  (add-hook 'prog-mode-hook 'fic-mode))

(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

(use-package highlight-escape-sequences
  :hook (prog-mode . hes-mode))
