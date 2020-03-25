(use-package yasnippet
  :defer t
  :requires helm
  :config
  (defun shk-yas/helm-prompt (prompt choices &optional display-fn)
    "Use helm to select a snippet. Put this into `yas/prompt-functions.'"
    (interactive)
    (setq display-fn (or display-fn 'identity))
    (if (require 'helm-config)
        (let (tmpsource cands result rmap)
          (setq cands (mapcar (lambda (x) (funcall display-fn x)) choices))
          (setq rmap (mapcar (lambda (x) (cons (funcall display-fn x) x)) choices))
          (setq tmpsource
                (list
                 (cons 'name prompt)
                 (cons 'candidates cands)
                 '(action . (("Expand" . (lambda (selection) selection))))
                 ))
          (setq result (helm-other-buffer '(tmpsource) "*helm-select-yasnippet"))
          (if (null result)
              (signal 'quit "user quit!")
            (cdr (assoc result rmap))))
      nil))
  (add-to-list 'yas-prompt-functions 'shk-yas/helm-prompt)

  (yas-global-mode 1)

  ;; Disable normal tab expansion because it often interferes with
  ;; indentation.
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "C-M-y") 'yas-expand)

  ;; Test if thing at cursor can expand with yas, if it can then change the color of the cursor.
  (setq default-cursor-color (face-background 'cursor))
  (setq yasnippet-can-fire-cursor-color "#4CB5F5")

  (defun yasnippet-can-fire-p (&optional field)
    (interactive)
    (setq yas--condition-cache-timestamp (current-time))
    (let (templates-and-pos)
      (unless (and yas-expand-only-for-last-commands
                   (not (member last-command yas-expand-only-for-last-commands)))
        (setq templates-and-pos (if field
                                    (save-restriction
                                      (narrow-to-region (yas--field-start field)
                                                        (yas--field-end field))
                                      (yas--templates-for-key-at-point))
                                  (yas--templates-for-key-at-point))))
      (and templates-and-pos (first templates-and-pos))))

  (defun my/change-cursor-color-when-can-expand (&optional field)
    (interactive)
    (when (eq last-command 'self-insert-command)
      (if (my/can-expand)
          (progn
            (set-cursor-color yasnippet-can-fire-cursor-color)
            ;; Change back 5 seconds afterwards to avoid border cases where it stays in the "can
            ;; expand" color when actually it can't.
            (run-at-time "5 sec" nil
                         (lambda ()
                           (set-cursor-color default-cursor-color))))
        (set-cursor-color default-cursor-color))))

  (defun my/can-expand ()
    "Return true if right after an expandable thing."
    (or (abbrev--before-point) (yasnippet-can-fire-p)))

  (add-hook 'post-command-hook 'my/change-cursor-color-when-can-expand))

(use-package helm-c-yasnippet
  :requires yasnippet
  :bind ("C-c y" . helm-yas-complete))

