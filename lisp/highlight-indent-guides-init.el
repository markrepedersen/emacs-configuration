(use-package highlight-indent-guides
  :if *sys/gui*
  :diminish
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character
	highlight-indent-guides-responsive 'top
	highlight-indent-guides-delay 0
	highlight-indent-guides-auto-character-face-perc 7))
