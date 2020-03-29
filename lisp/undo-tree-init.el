(use-package undo-tree
  :defer t
  :bind (("C-z" . undo)
	 ("C-S-z" . undo-tree-redo)
	 ("C-c z" . undo-tree-visualize))
  :config
  (global-undo-tree-mode)
  (lambda() (defadvice undo-tree-make-history-save-file-name
		(after undo-tree activate)
	      (setq ad-return-value (concat ad-return-value ".gz"))))
  (setq undo-tree-auto-save-history t
	undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))
