(use-package undo-tree
  :defer t
  :config
  (global-undo-tree-mode)
  (lambda() (defadvice undo-tree-make-history-save-file-name
		(after undo-tree activate)
	      (setq ad-return-value (concat ad-return-value ".gz"))))
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-S-z") 'undo-tree-redo)
(global-set-key (kbd "C-x z") 'undo-tree-visualize)
