(use-package centaur-tabs
  :hook
  (dashboard-mode . centaur-tabs-local-mode)
  (term-mode . centaur-tabs-local-mode)
  (shell-mode . centaur-tabs-local-mode)
  (vterm-mode . centaur-tabs-local-mode)
  (calendar-mode . centaur-tabs-local-mode)
  (fundamental-mode . centaur-tabs-local-mode)
  (Tooltip . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
  (helpful-mode . centaur-tabs-local-mode)
  :bind
  ("C-x p" . centaur-tabs-backward)
  ("C-S-x p" . centaur-tabs-backward-group)
  ("C-x n" . centaur-tabs-forward)
  ("C-S-x n" . centaur-tabs-forward-group)
  :demand
  :config
  (centaur-tabs-mode t)
  (centaur-tabs-headline-match)
  (centaur-tabs-group-by-projectile-project)

  (defun vmacs-term-mode-p(&optional args)
    (derived-mode-p 'eshell-mode 'term-mode 'shell-mode 'vterm-mode))

  (setq centaur-tabs-style "bar"
	centaur-tabs-gray-out-icons 'buffer
	centaur-tabs-set-bar 'over
	centaur-tabs-set-close-button nil
	centaur-tabs-height 32
	centaur-tabs-show-navigation-buttons nil
	centaur-tabs-set-modified-marker t
	centaur-tabs-set-icons t
	vterm-toggle--vterm-buffer-p-function 'vmacs-term-mode-p))
