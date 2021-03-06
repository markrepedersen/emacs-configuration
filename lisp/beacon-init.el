(use-package beacon
  :config
  (beacon-mode 1)
  (defun backward-paragraph-blink ()
    (interactive)
    (backward-paragraph)
    (beacon-blink))
  (defun forward-paragraph-blink ()
    (interactive)
    (forward-paragraph)
    (beacon-blink))
  (global-set-key (kbd "M-p") 'backward-paragraph-blink)
  (global-set-key (kbd "M-n") 'forward-paragraph-blink)
  (setq beacon-color "green")
  (setq beacon-blink-duration 0.1)
  (setq beacon-blink-when-point-moves-vertically 2)
  (setq beacon-blink-when-point-moves-horizontally 2)
  (setq beacon-blink-when-buffer-changes t)
  (setq beacon-blink-when-window-scrolls t)
  (setq beacon-size 40))
