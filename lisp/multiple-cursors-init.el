(use-package multiple-cursors
  :defer t
  :bind (("C->" . mc/mark-next-like-this)
	 ("C-<" . mc/mark-previous-like-this)
	 ("C-c C->" . mc/mark-all-like-this))
  :config
  ;; These modes make mc very slow, so let's get rid of them while mc is on.
  (push '(flyspell-mode smartparens-mode linum-mode flycheck-mode company-mode company-box-mode) mc/unsupported-minor-modes))
