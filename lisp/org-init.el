(use-package org-mode
  :ensure nil
  :defer t
  :after hydra
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
		    ("<SPC>" nil nil))
  :init
  (setq my-org-dir "~/notes/org/"
	my-org-schedule-file-path (concat my-org-dir "schedule.org")
	org-directory my-org-dir
	org-default-notes-file (concat org-directory "notes.org")
	org-refile-targets '(("notes.org" :maxlevel . 6))
	org-completion-use-ido t)
  :config
  (org-agenda-start-with-follow-mode t)
  (setq	org-agenda-files (list my-org-schedule-file-path)
	org-capture-templates '(("a" "Appointment" entry (file my-org-schedule-file-path)
				 "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")
				("t" "Todo" entry (file+headline (concat my-org-dir "todo.org") "Todo")
				 "* TODO %?\n%u" :prepend t)
				("n" "Note" entry (file+headline "~" "Note space")
				 "* %?\n%u" :prepend t)))
  (use-package org-gcal
    :defer t
    :hook ((org-agenda-mode-hook . (lambda () (org-gcal-sync)))
	   (org-capture-after-finalize-hook . (lambda () (org-gcal-sync))))
    :config
    (setq org-gcal-client-id "54453394323-s2irq9p2q9tgf06crqt8ch25pidjoqqk.apps.googleusercontent.com"
	  org-gcal-client-secret (auth-source-pass-get 'secret "email/gmail/oauth")
	  org-gcal-file-alist '(("markrepedersen@gmail.com" .  my-org-schedule-file-path)))))
