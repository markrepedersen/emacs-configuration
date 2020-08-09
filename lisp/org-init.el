(defvar markrepedersen/org-dir "~/notes/org")
(defvar markrepedersen/org-attachments-dir (concat markrepedersen/org-dir "/attachments/"))
(defvar markrepedersen/org-appointments-file (concat markrepedersen/org-dir "/appointment.org"))
(defvar markrepedersen/org-projects-file (concat markrepedersen/org-dir "/project.org"))

(use-package org
  :bind (("C-c o" . org-mode-hydra/body))
  :preface
  (defun create-blog-post(post-name)
    "Create a new blog post with the given name under my website's repository."
    (interactive "sPost's name: ")
    (switch-to-buffer
     (generate-new-buffer (expand-file-name
			   (concat (format-time-string "%Y-%m-%d-") blog-name ".org")
			   markrepedersen/website-dir))))
  :config
  (setq org-directory markrepedersen/org-dir
	org-startup-with-inline-images t
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
				 :empty-lines 1))
	org-publish-project-alist '(("org-markrepedersen"
				     :base-directory "~/work/markrepedersen.github.io/org/"
				     :base-extension "org"
				     :publishing-directory "~/work/markrepedersen.github.io/docs"
				     :recursive t
				     :publishing-function org-html-publish-to-html
				     :headline-levels 4
				     :html-extension "html"
				     :body-only t)
				    ("org-static-markrepedersen"
				     :base-directory "~/work/markrepedersen.github.io/org"
				     :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|php\\|mov\\|html\\|txt\\|"
				     :publishing-directory "~/work/markrepedersen.github.io/docs"
				     :recursive t
				     :publishing-function org-publish-attachment)
				    ("markrepedersen.github.io" :components ("org-markrepedersen" "org-static-markrepedersen")))))

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


(pretty-hydra-define org-mode-functions
  (:quit-key "q" :title (with-mode-icon 'org-mode "Org mode"))
  ("Clock"
   (("i" (lambda () (interactive) (org-clock-in '(4))) "Clock in")
    ("o" org-clock-out "Clock out")
    ("q" org-clock-cancel "Cancel clock")
    ("b" org-clock-in-last "Clock in last")
    ("j" (lambda () (interactive) (org-clock-goto '(4))) "Go to clock"))
   "Website"
   (("C-c" create-blog-post "Create blog post"))
   "Capture"
   (("c" org-capture "Capture")
    ("l" org-capture-goto-last-stored "Last capture"))
   "Agenda"
   (("a" org-agenda "Agenda"))
   "Structure"
   (("s" org-insert-structure-template "Templates"))
   "Table"
   (("t" org-table-create-or-convert-from-region "Create table")
    ("r" org-table-insert-row "Create row")
    ("c" org-table-insert-column "Create column")
    ("h" org-table-insert-hline "Create header"))))

(global-set-key (kbd "C-c o") 'org-mode-functions/body)
