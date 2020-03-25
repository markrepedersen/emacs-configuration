(use-package org-mode
  :after hydra
  :ensure nil
  :config
  (setq org-agenda-files '("~/Dropbox/org/"))
  (setq org-directory "~/Dropbox/org")
  (setq org-default-notes-file (concat org-directory "~/Dropbox/org/notes.org"))
  (setq org-refile-targets '(("notes.org" :maxlevel . 6)))
  (setq org-completion-use-ido t)
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)
  :hook flyspell-mode
  :bind (("C-c o" . hydra-org/body))
  :hydra (hydra-org (:exit t :hint nil :color blue)
		    "Org mode"
		    ("i" (lambda () (interactive) (org-clock-in '(4))) "Clock in" :column "Clock")
		    ("o" org-clock-out "Clock out")
		    ("q" org-clock-cancel "Cancel clock")
		    ("<f10>" org-clock-in-last "Clock in last")
		    ("j" (lambda () (interactive) (org-clock-goto '(4))) "Go to clock")
		    ("c" org-capture "Capture" :column "Capture")
		    ("l" org-capture-goto-last-stored "Last capture")
		    ("<SPC>" nil nil)))
