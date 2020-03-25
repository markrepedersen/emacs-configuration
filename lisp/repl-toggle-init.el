(use-package repl-toggle
  :defer t
  :custom
  (rtog/mode-repl-alist
   '((emacs-lisp-mode . ielm)
     (python-mode . elpy-shell-switch-to-shell)
     (js-mode . nodejs-repl)
     (typescript-mode . run-ts)))
  :config
  (setq rtog/fullscreen nil)
  (repl-toggle-mode))
