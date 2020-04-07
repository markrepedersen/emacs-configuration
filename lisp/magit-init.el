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
    (("c" magit-commit "commit")
     ("r" magit-rebase "rebase"))
    "Status"
    (("s" magit-status "status")
     ("l" magit-log "log"))
    "Clean"
    (("C-c" (lambda () (magit-clean "-dffx")) "(-dffx)")
     ("C-d" (lambda () (magit-clean "-dffxn")) "(-dffxn)")
     ("C-r" magit-reset "reset"))
    "Checkout"
    (("C-b" magit-checkout "branch")
     ("C-f" magit-file-checkout "file"))
    "Branches"
    (("b" magit-branch "branch")
     ("d" magit-branch-delete "delete"))
    "Update"
    (("C" magit-clone "clone")
     ("P" magit-pull "pull")
     ("p" magit-push "push")
     ("f" magit-fetch "fetch"))))
  :bind (("C-c g" . magit-hydra/body))
  :config
  (put 'magit-clean 'disabled nil))
