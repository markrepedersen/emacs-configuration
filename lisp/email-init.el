(use-package mu4e
  :load-path "/usr/local/share/emacs/site-lisp/mu/mu4e"
  :commands (mu4e mu4e-headers-search mu4e-compose-new mu4e~proc-add)
  :hook ((mu4e-compose-mode-hook . flycheck-mode)
	 (mu4e-view-mode-hook . (lambda()
				  (local-set-key (kbd "<RET>") 'mu4e~view-browse-url-from-binding)
				  (local-set-key (kbd "<tab>") 'shr-next-link)
				  (local-set-key (kbd "<backtab>") 'shr-previous-link))))
  :config
  (defun markrepedersen/mu4e-action-view-in-browser-webkit (msg)
    (let ((url (concat "file://" (mu4e~write-body-to-html msg))))
      (xwidget-webkit-browse-url url)))

  (add-to-list 'mu4e-view-actions '("View in Browser" . markrepedersen/mu4e-action-view-in-browser-webkit) t)

  (when (executable-find "w3m")
    (setq mu4e-view-prefer-html t)
    (setq mu4e-html2text-command "w3m -dump -T text/html"))

  (setq mail-user-agent 'mu4e-user-agent
	mu4e-index-cleanup nil ;; don't do a full cleanup check
	mu4e-index-lazy-check t
	mu4e-mu-binary "/usr/local/bin/mu"
	message-kill-buffer-on-exit t
	mu4e-sent-messages-behavior 'delete
	mu4e-attachment-dir (expand-file-name "~/Downloads")
	mu4e-compose-signature-auto-include nil
	mu4e-get-mail-command "mbsync -a"
	mu4e-maildir (expand-file-name "~/mail")
	mu4e-headers-fields '((:human-date .  12) ;; alternatively, use :human-date
                              ;; (:flags      .   6)
                              (:from       .  24)
                              (:subject    .  nil))
	mu4e-maildir-shortcut '(("/gmail/inbox" . ?i)
				("/gmail/archived" . ?a)
				("/gmail/trash" . ?d)
				("/gmail/drafts" . ?D)
				("/gmail/important" . ?i)
				("/gmail/sent" . ?s)
				("/gmail/starred" . ?S))
	mu4e-trash-folder "/gmail/trash"
	mu4e-drafts-folder "/gmail/drafts"
	mu4e-refile-folder "/gmail/archived"
	mu4e-sent-folder "/gmail/sent"
	mu4e-update-interval 10
	mu4e-use-fancy-chars t
	mu4e-view-show-addresses t
	mu4e-change-filenames-when-moving t
	mu4e-view-show-images t
	mu4e-confirm-quit nil
	mu4e-split-view 'vertical
	mu4e-headers-skip-duplicates t
	mu4e-hide-index-messages t
	org-mu4e-convert-to-html t
	;; Signature
	user-mail-address "markrepedersen@gmail.com"
	user-full-name  "Mark"
	;; For setting up sending emails
	message-send-mail-function 'smtpmail-send-it
	smtpmail-default-smtp-server "smtp.gmail.com"
	smtpmail-smtp-server "smtp.gmail.com"
	smtpmail-smtp-service 587
	mu4e-compose-signature (concat "Mark Pedersen")))

(use-package mu4e-alert
  :after mu4e
  :config
  (mu4e-alert-enable-mode-line-display)
  (mu4e-alert-enable-notifications)
  (mu4e-alert-set-default-style 'libnotify)
  (setq mu4e-alert-interesting-mail-query (concat
					   "flag:unread"
					   " AND NOT flag:trashed"
					   " AND maildir:/INBOX")))

(use-package helm-mu
  :after helm)

(pretty-hydra-define email-functions
  (:color teal :title (with-faicon "toggle-on" "Email" 1 -0.05))
  ("Bookmarks"
   (("s" (mu4e-headers-search) "query")
    ("i" (mu4e-headers-search "maildir:/gmail/inbox") "inbox")
    ("u" (mu4e-headers-search "flag:unread AND maildir:/gmail/inbox") "unread")
    ("T" (mu4e-headers-search "date:today..now AND maildir:/gmail/inbox") "today")
    ("W" (mu4e-headers-search "date:1w.. AND maildir:/gmail/inbox") "this week")
    ("M" (mu4e-headers-search "date:1m..  AND maildir:/gmail/inbox") "this month"))
   "Compose"
   (("C" (mu4e-compose-new) "New"))))

(global-set-key (kbd "C-c x") 'email-functions/body)
