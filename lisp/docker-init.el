(use-package docker
  :bind ("C-c d" . docker)
  :diminish)

(use-package docker-compose-mode
  :mode "docker-compose.*\.yml\\'")

(use-package docker-tramp
  :after tramp
  :defer 5)

(use-package dockerfile-mode
  :mode "Dockerfile[a-zA-Z.-]*\\'")
