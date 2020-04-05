(use-package emacs-lisp-mode
  :ensure nil
  :mode-hydra
  ("Eval"
   (("b" eval-buffer "buffer")
    ("e" eval-defun "defun")
    ("r" eval-region "region"))
   "REPL"
   (("I" ielm "ielm"))
   "Test"
   (("t" ert "prompt")
    ("T" (ert t) "all")
    ("F" (ert :failed) "failed"))
   "Doc"
   (("d" describe-foo-at-point "thing-at-pt")
    ("f" counsel-describe-function "function")
    ("v" counsel-describe-variable "variable")
    ("i" info-lookup-symbol "info lookup"))
   "Quit"
   (("q" nil "quit hydra"))))
