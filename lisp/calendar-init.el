(use-package calfw
  :commands cfw:open-calendar-buffer
  :bind ("<C-c C-c C-a>" . open-calendar)
  :init
  (use-package calfw-org
    :commands (cfw:open-org-calendar cfw:org-create-source))

  (use-package calfw-ical
    :commands (cfw:open-ical-calendar cfw:ical-create-source))

  (defun open-calendar ()
    "Open calendar."
    (interactive)
    (unless (ignore-errors
              (cfw:open-calendar-buffer
               :contents-sources
               (list
                (when org-agenda-files
                  (cfw:org-create-source "YellowGreen"))
                (when (bound-and-true-p centaur-ical)
                  (cfw:ical-create-source "gcal" centaur-ical "IndianRed")))))
      (cfw:open-calendar-buffer)))
  (defalias 'centaur-open-calendar #'open-calendar))
