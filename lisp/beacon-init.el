(use-package beacon
  :config
  (setq beacon-blink-delay 0.1
        beacon-blink-duration 0.3
        beacon-color "#b1d631")

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

  (beacon-mode 1))
