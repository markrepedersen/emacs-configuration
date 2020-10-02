(use-package centaur-tabs
  :hook
  (dashboard-mode . centaur-tabs-local-mode)
  (term-mode . centaur-tabs-local-mode)
  (shell-mode . centaur-tabs-local-mode)
  (vterm-mode . centaur-tabs-local-mode)
  (calendar-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
  (helpful-mode . centaur-tabs-local-mode)
  :bind
  ("C-x p" . centaur-tabs-backward)
  ("C-x n" . centaur-tabs-forward)
  :demand
  :config
  (centaur-tabs-mode t)
  (centaur-tabs-headline-match)

  (setq centaur-tabs-style "bar"
	centaur-tabs-height 32
	centaur-tabs-show-navigation-buttons t
	centaur-tabs-set-modified-marker t
	centaur-tabs-set-icons t
	centaur-tabs-set-bar 'left)

  (defun centaur-tabs-buffer-groups ()
    "`centaur-tabs-buffer-groups' control buffers' group rules.

 Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
 All buffer name start with * will group to \"Emacs\".
 Other buffer group by `centaur-tabs-get-group-name' with project name."
    (list
     (cond
      ;; ((not (eq (file-remote-p (buffer-file-name)) nil))
      ;; "Remote")
      ((or (string-equal "*" (substring (buffer-name) 0 1))
	   (memq major-mode '(magit-process-mode
			      magit-status-mode
			      magit-diff-mode
			      magit-log-mode
			      magit-file-mode
			      magit-blob-mode
			      magit-blame-mode
			      )))
       "Emacs")
      ((derived-mode-p 'prog-mode)
       "Editing")
      ((derived-mode-p 'dired-mode)
       "Dired")
      ((memq major-mode '(helpful-mode
			  help-mode))
       "Help")
      ((memq major-mode '(org-mode
			  org-agenda-clockreport-mode
			  org-src-mode
			  org-agenda-mode
			  org-beamer-mode
			  org-indent-mode
			  org-bullets-mode
			  org-cdlatex-mode
			  org-agenda-log-mode
			  diary-mode))
       "OrgMode")
      (t
       (centaur-tabs-get-group-name (current-buffer))))))

  (setq vterm-toggle--vterm-buffer-p-function 'vmacs-term-mode-p)
  (defun vmacs-term-mode-p(&optional args)
    (derived-mode-p 'eshell-mode 'term-mode 'shell-mode 'vterm-mode)))
