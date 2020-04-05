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
  ((:color teal :quit-key "q" :title (markrepedersen/current-git-repo))
   ("Commit"
    (("c" magit-commit "commit")
     ("s" magit-status "status")
     ("l" magit-log "log")
     ("r" magit-rebase "rebase")
     ("R" magit-reset "reset"))
    "Branches"
    (("b" magit-branch "branch")
     ("d" magit-branch-delete "delete"))
    "Update"
    (("C" magit-clone "clone")
     ("P" magit-pull "pull")
     ("p" magit-push "push")
     ("f" magit-fetch "fetch popup"))))
  :bind (("C-c g" . magit-hydra/body)))
