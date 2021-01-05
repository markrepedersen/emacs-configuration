(use-package repl-toggle
  :defer t
  :bind (("C-c C-r" . rtog/toggle-repl))
  :config
  (setq rtog/fullscreen t
	rtog/mode-repl-alist '((emacs-lisp-mode . ielm)
			       (python-mode . elpy-shell-switch-to-shell)
			       (js-mode . nodejs-repl)
			       (typescript-mode . run-ts)))
  (repl-toggle-mode))
