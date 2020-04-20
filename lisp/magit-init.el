(defun markrepedersen/current-git-repo ()
  (let ((p (projectile-project-name)))
    (s-concat
     (all-the-icons-alltheicon "git")
     " "
     (if (s-blank-p p)
	 "Git"
       (s-concat "Git repo: " p)))))

(use-package magit
  :after hydra
  :pretty-hydra
  ((:color teal :title (markrepedersen/current-git-repo))
   ("Commit"
    (("cc" magit-commit "commit")
     ("cr" magit-rebase "rebase"))
    "Stash"
    (("ss" magit-stash "options"))
    "Diff"
    (("dd" magit-diff "options"))
    "Status"
    (("ss" magit-status "status"))
    "Log"
    (("ls" magit-log-head "head")
     ("lm" magit-log-merged "merged"))
    "Clean"
    (("Cc" (lambda () (interactive) (magit-clean "-dffx")) "(-dffx)")
     ("Cn" (lambda () (interactive) (magit-clean "-dffxn")) "(-dffxn)")
     ("Cr" magit-reset "reset"))
    "Checkout"
    (("Cb" magit-checkout "branch")
     ("Cf" magit-file-checkout "file"))
    "Branches"
    (("bx" magit-branch "branch")
     ("bd" magit-branch-delete "delete"))
    "Update"
    (("uc" magit-clone "clone")
     ("up" magit-pull "pull")
     ("up" magit-push "push")
     ("uf" magit-fetch "fetch"))))
  :bind (("C-c g" . magit-hydra/body))
  :config
  (put 'magit-clean 'disabled nil))
