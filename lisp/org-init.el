(defvar markrepedersen/org-dir "~/notes/org")
(defvar markrepedersen/org-attachments-dir (concat markrepedersen/org-dir "/attachments/"))
(defvar markrepedersen/org-appointments-file (concat markrepedersen/org-dir "/appointment.org"))
(defvar markrepedersen/org-projects-file (concat markrepedersen/org-dir "/project.org"))

(use-package org
  :after hydra
  :bind (("C-c o" . hydra-org/body))
  :hydra (hydra-org (:hint nil :color blue)
		    "Org mode"
		    ("i" (lambda () (interactive) (org-clock-in '(4))) "Clock in" :column "Clock")
		    ("o" org-clock-out "Clock out")
		    ("q" org-clock-cancel "Cancel clock")
		    ("b" org-clock-in-last "Clock in last")
		    ("j" (lambda () (interactive) (org-clock-goto '(4))) "Go to clock")
		    ("c" org-capture "Capture" :column "Capture")
		    ("l" org-capture-goto-last-stored "Last capture")
		    ("a" org-agenda "Agenda" :column "Agenda"))
  :config
  (setq org-directory markrepedersen/org-dir
	org-attach-id-dir markrepedersen/org-attachments-dir
	org-agenda-files `(,org-directory)
	org-return-follows-link t
	org-completion-use-ido t
	org-todo-keywords '((sequence "SCHEDULED" "IN-PROGRESS" "|" "CANCELLED" "DONE")
			    (sequence "BUG" "KNOWN_CAUSE" "|" "DONE"))
	org-todo-keyword-faces '(("IN_PROGRESS" . "orange")
				 ("CANCELED" . "red")
				 ("SCHEDULED" . "white")
				 ("BUG" . "maroon")
				 ("KNOWN_CAUSE" . "purple")
				 ("DONE" . "green"))
	org-capture-templates `(("a"              ; key
				 "Appointment"    ; description
				 entry            ; type
				 (file+headline markrepedersen/org-appointments-file "Appointments")
				 "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n"
				 :empty-lines 1)    ; properties
				("p"
				 "Project"
				 entry
				 (file+headline markrepedersen/org-projects-file "Projects")
				 "* TODO %?\n %^{Project}p\nDEADLINE: %^t\n"
				 :empty-lines 1))))

(use-package org-gcal
  :hook ((org-agenda-mode . org-gcal-sync)
	 (org-capture-after-finalize . org-gcal-sync))
  :config
  (setq org-gcal-client-id (auth-source-pass-get "username" "email/gmail/oauth")
	org-gcal-client-secret (auth-source-pass-get 'secret "email/gmail/oauth")
	org-gcal-file-alist `(("markrepedersen@gmail.com" .  ,markrepedersen/org-appointments-file))))

(use-package org-super-agenda
  :after org-agenda
  :config
  (setq org-super-agenda-groups '((:name "Today"
					 :time-grid t
					 :scheduled today)
				  (:name "Due today"
					 :deadline today)
				  (:name "Important"
					 :priority "A")
				  (:name "Overdue"
					 :deadline past)
				  (:name "Due soon"
					 :deadline future)
				  (:name "Waiting"
					 :todo "WAIT"))))
