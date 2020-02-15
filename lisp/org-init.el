(use-package org-mode
  :ensure f
  :init
  (setq org-directory "~/notes/org")
  (setq org-default-notes-file (concat org-directory "/notes/notes.org"))
  (setq org-refile-targets '(("notes.org" :maxlevel . 6)))
  (setq org-completion-use-ido t)
  ;;https://lists.gnu.org/archive/html/emacs-orgmode/2008-05/msg00039.html
  (defun my-link-to-line-number-in-c-mode ()
    "When in c-mode, use line number as search item."
    (when (eq major-mode 'c-mode)
      (number-to-string (org-current-line))))

  (add-hook 'org-create-file-search-functions
            'my-link-to-line-number-in-c-mode)

  (setq org-capture-templates
        '(("c" "Code" entry (file+headline "~/notes/notes.org" "Scratch")
           "** Snippet\n %i\n%a")))
  (add-hook 'org-mode-hook 'flyspell-mode)
  :bind ("C-c c" . org-capture)
  )
