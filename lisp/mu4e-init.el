(use-package mu4e
  :demand
  :load-path "/usr/local/share/emacs/site-lisp/mu/mu4e"
  :hook ((mu4e-compose-pre-hook . my-mu4e-set-account))
  :init
  (setq mu4e-attachment-dir "~/Downloads"
	mu4e-compose-signature-auto-include nil
	mu4e-drafts-folder "/gmail/Drafts"
	mu4e-maildir "~/Maildir"
	mu4e-refile-folder "/gmail/Archive"
	mu4e-maildir-shortcuts
	'(("/gmail/Inbox" . ?i)
	  ("/gmail/All Mail" . ?a)
	  ("/gmail/Deleted Items" . ?d)
	  ("/gmail/Drafts" . ?D)
	  ("/gmail/Important" . ?i)
	  ("/gmail/Sent Mail" . ?s)
	  ("/gmail/Starred" . ?S))
	mu4e-trash-folder "/gmail/Trash"
	mu4e-update-interval 300
	mu4e-use-fancy-chars t
	mu4e-view-show-addresses t
	mu4e-view-show-images t
	mu4e-completing-read-function 'completing-read
	message-kill-buffer-on-exit t
	mu4e-context-policy 'pick-first
	mu4e-confirm-quit nil
	user-mail-address "markrepedersen@gmail.com"
	smtpmail-default-smtp-server "smtp.gmail.com"
	smtpmail-smtp-server "smtp.gmail.com"
	smtpmail-smtp-service 587)
  (defvar my-mu4e-account-alist
    '(("Gmail"
       (user-mail-address "markrepedersen@gmail.com")
       (smtpmail-smtp-user "markrepedersen")
       (smtpmail-local-domain "gmail.com")
       (smtpmail-default-smtp-server "smtp.gmail.com")
       (smtpmail-smtp-server "smtp.gmail.com")
       (smtpmail-smtp-service 587))
      ;; Include any other accounts here ...
      ))
  (defun my-mu4e-set-account ()
    (let* ((account
	    (if mu4e-compose-parent-message
		(let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
		  (string-match "/\\(.*?\\)/" maildir)
		  (match-string 1 maildir))
	      (completing-read (format "Compose with account: (%s) "
				       (mapconcat #'(lambda (var) (car var))
						  my-mu4e-account-alist "/"))
			       (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
			       nil t nil nil (caar my-mu4e-account-alist))))
	   (account-vars (cdr (assoc account my-mu4e-account-alist))))
      (if account-vars
	  (mapc #'(lambda (var)
		    (set (car var) (cadr var)))
		account-vars)
	(error "No email account found")))))

(use-package mu4e-alert
  :after mu4e
  :hook ((after-init . mu4e-alert-enable-mode-line-display)
         (after-init . mu4e-alert-enable-notifications))
  :config (mu4e-alert-set-default-style 'libnotify))
