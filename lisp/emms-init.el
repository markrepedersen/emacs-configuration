(use-package emms
  :defer t
  :after hydra
  :hydra (hydra-emms (:color blue :columns 3)
		     "Emacs Multimedia Player"
		     ("x" (emms-play-directory-tree "~/Music") "Play All" :column "Play")
		     ("g" emms-playlist-mode-go "View All")
		     ("n" emms-next "Next" :column "Move")
		     ("p" emms-previous "Previous")
		     ("P" emms-pause "Pause")
		     ("+" emms-volume-mode-plus "Volume+" :column "Volume")
		     ("-" emms-volume-mode-minus "Volume-"))
  :bind ("C-c i" . hydra-emms/body)
  :config
  (require 'emms-setup)
  (emms-all)
  (emms-default-players)
  (setq emms-playlist-default-major-mode 'emms-playlist-mode)
  (setq emms-source-file-default-directory "~/Music/")
  (setq emms-playlist-buffer-name "*Shitty-Ass Music...*")
  (setq emms-info-asynchronously t)
  (require 'emms-mode-line)
  (emms-mode-line 1)
  (require 'emms-playing-time)
  (emms-playing-time 1)
  (emms-add-directory-tree "~/Music/"))
