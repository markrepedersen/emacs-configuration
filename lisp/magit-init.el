(use-package magit
  :defer t
  :after hydra
  :hydra (hydra-magit (:hint nil :exit t)
		      "Git"
		      ("c" magit-commit "commit" :column "Local")
		      ("s" magit-status "status")
		      ("l" magit-log "log")
		      ("b" magit-branch "branch")
		      ("r" magit-rebase "rebase")
		      ("R" magit-reset "reset")
		      ("C" magit-clone "clone" :column "Server")
		      ("P" magit-pull "pull" :column "Update")
		      ("p" magit-push "push")
		      ("f" magit-fetch "fetch popup"))
  :bind (("C-c g" . hydra-magit/body)))
