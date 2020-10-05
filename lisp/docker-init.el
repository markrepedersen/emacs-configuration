(use-package docker
  :defer t
  :bind ("C-c d" . docker)
  :diminish)

(use-package docker-compose-mode
  :defer t
  :mode "docker-compose[a-zA-Z.-]*\\'"
  :mode-hydra
  ((:color teal :quit-key "q" :title (with-mode-icon 'dockerfile-mode "Docker-compose mode"))
   ("Docker Compose"
    (("d" docker-compose "Options")))))

(use-package docker-tramp
  :after tramp
  :defer t)

(use-package dockerfile-mode
  :defer t
  :mode "Dockerfile[a-zA-Z.-]*\\'"
  :config
  (put 'dockerfile-image-name 'safe-local-variable #'stringp)
  :mode-hydra
  ((:color teal :quit-key "q" :title (with-mode-icon 'dockerfile-mode "Dockerfile mode"))
   ("Build"
    (("b" dockerfile-build-buffer "Build")
     ("c" dockerfile-build-no-cache-buffer "Build (no cache)")))))
