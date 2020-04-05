(use-package projectile
  :config
  (setq projectile-enable-caching t
	projectile-sort-order 'recently-active
	projectile-file-exists-remote-cache-expire nil)
  (projectile-mode))

(use-package helm-projectile
  :config
  (helm-projectile-on)
  (defun markrepedersen/current-project-title ()
    (let ((p (projectile-project-name)))
      (with-octicon "repo"
                    (if (s-blank-p p)
			"Projects"
                      (s-concat "Projects (current: " p ")")))))
  (pretty-hydra-define marks-projects
    (:color teal :quit-key "q" :title (markrepedersen/current-project-title))
    ("All Projects"
     (("a" helm-projectile "all" :column "Show all")
      ("p" helm-projectile-switch-project "switch project")
      ("b" projectile-save-project-buffers "save buffers"))
     "Current Project"
     (("f" helm-projectile-find-file "find file"))))
  (global-set-key (kbd "C-c p") 'marks-projects/body))
