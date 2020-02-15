(use-package magit
  :commands magit-get-top-dir)

(defhydra hydra-magit (:color blue
                              :columns 4)
  "Magit"
  ("c" magit-commit "commit")
  ("p" magit-push "push")
  ("s" magit-status "status")
  ("l" magit-log-all-branches "log")
  ("b" magit-branch-popup "branch popup")
  ("r" magit-rebase-popup "rebase popup")
  ("f" magit-fetch-popup "fetch popup")
  ("P" magit-push-popup "push popup")
  ("F" magit-pull-popup "pull popup")
  ("W" magit-format-patch "format patch")
  ("$" magit-process "process"))

(global-set-key (kbd "C-c C-c g") 'hydra-magit/body)
