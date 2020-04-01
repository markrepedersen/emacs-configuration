(use-package repl-toggle
  :bind (("C-c C-'" . repl-toggle-mode))
  :config
  (setq rtog/mode-repl-alist
   '((emacs-lisp-mode . ielm)
     (python-mode . elpy-shell-switch-to-shell)
     (js-mode . nodejs-repl)
     (typescript-mode . run-ts))
   rtog/fullscreen nil)
  (repl-toggle-mode))
