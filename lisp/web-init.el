(use-package web-mode
  :mode (("\\.js\\'" . web-mode)
         ("\\.jsx\\'" . web-mode)
         ("\\.ts\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)
         ("\\.html\\'" . web-mode)
         ("\\.vue\\'" . web-mode)
	 ("\\.json\\'" . web-mode))
  :hook
  ((web-mode . lsp)
   (web-mode . (lambda() (local-unset-key (kbd "C-c C-f")))))
  :commands web-mode
  :config
  (setq    web-mode-markup-indent-offset 2
	   web-mode-code-indent-offset 2
	   web-mode-enable-auto-indentation t
	   web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'"))))
