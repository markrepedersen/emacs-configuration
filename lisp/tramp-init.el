(use-package tramp
  :init
  (setq password-cache-expiry nil
	tramp-use-ssh-controlmaster-options nil
	vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)" vc-ignore-dir-regexp tramp-file-name-regexp)))
