(defun duplicate-line-or-region (&optional n)
  "Duplicate current line, or region if active."
  (interactive "*p")
  (let ((use-region (use-region-p)))
    (save-excursion
      (let ((text (if use-region        ;Get region if active, otherwise line
                      (buffer-substring (region-beginning) (region-end))
                    (prog1 (thing-at-point 'line)
                      (end-of-line)
                      (if (< 0 (forward-line 1)) ;Go to beginning of next line, or make a new one
                          (newline))))))
        (dotimes (i (abs (or n 1)))     ;Insert N times, or once if not specified
          (insert text))))
    (if use-region nil                  ;Only if we're working with a line (not a region)
      (let ((pos (- (point) (line-beginning-position)))) ;Save column
        (if (> 0 n)                             ;Comment out original with negative arg
            (comment-region (line-beginning-position) (line-end-position)))
        (forward-line 1)
        (forward-char pos)))))

(defun align-repeat ()
  (interactive)
  (align-regexp (point-min)
		(point-max)
		(concat "\\(\\s-*\\)" "[[:space:]]+") 1 1 t))

(global-set-key (kbd "C-c C-c i") 'align-repeat)
(global-set-key (kbd "C-d") 'duplicate-line-or-region)
(global-set-key (kbd "C-k") 'kill-whole-line)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-0") 'text-scale-adjust)
(global-set-key (kbd "C-.") 'pop-to-mark-command)
(global-set-key (kbd "M-;") 'comment-line)
