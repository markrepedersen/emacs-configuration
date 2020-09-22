(use-package mu4e
  :load-path "/usr/local/share/emacs/site-lisp/mu/mu4e"
  :config
  (setq mail-user-agent 'mu4e-user-agent
	mu4e-mu-binary "/usr/local/bin/mu"
	message-kill-buffer-on-exit t
	mu4e-sent-messages-behavior 'delete
	mu4e-attachment-dir "~/Downloads"
	mu4e-compose-signature-auto-include nil
	mu4e-drafts-folder "/Gmail/Drafts"
	mu4e-get-mail-command "mbsync -a"
	mu4e-maildir "~/Maildir"
	mu4e-refile-folder "/Gmail/Archive"
	mu4e-sent-folder "/Gmail/Sent"
	mu4e-maildir-shortcut
	'(("/Gmail/INBOX" . ?i)
	  ("/Gmail/All Mail" . ?a)
	  ("/Gmail/Deleted" . ?d)
	  ("/Gmail/Drafts" . ?D)
	  ("/Gmail/Important" . ?i)
	  ("/Gmail/Sent Mail" . ?s)
	  ("/Gmail/Starred" . ?S))
	mu4e-trash-folder "/Gmail/Trash"
	mu4e-update-interval 300
	mu4e-use-fancy-chars t
	mu4e-view-show-addresses t
	mu4e-view-show-images t
	mu4e-confirm-quit nil
	;; Signature
	user-mail-address "markrepedersen@gmail.com"
	user-full-name  "Mark"
	;; For setting up sending emails
	smtpmail-default-smtp-server "smtp.gmail.com"
	smtpmail-smtp-server "smtp.gmail.com"
	smtpmail-smtp-service 587
	mu4e-compose-signature
	(concat
	 "Mark Pedersen")))

(use-package mu4e-alert
  :after mu4e
  :hook ((after-init . mu4e-alert-enable-mode-line-display)
         (after-init . mu4e-alert-enable-notifications))
  :config (mu4e-alert-set-default-style 'libnotify))

(pretty-hydra-define email-functions
  (:title (with-faicon "toggle-on" "Toggles" 1 -0.05))
  ("Bookmarks"
   (("s" (mu4e-headers-search) "query")
    ("u" (mu4e-headers-search "flag:unread AND maildir:/Gmail/INBOX") "unread")
    ("T" (mu4e-headers-search "date:today..now AND maildir:/Gmail/INBOX") "today")
    ("W" (mu4e-headers-search "date:1w.. AND maildir:/Gmail/INBOX") "this week")
    ("M" (mu4e-headers-search "date:1m..  AND maildir:/Gmail/INBOX") "this month"))
   "Compose"
   (("C" (mu4e-compose-new) "New"))))

(global-set-key (kbd "C-c x") 'email-functions/body)
