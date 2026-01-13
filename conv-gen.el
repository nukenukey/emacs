;; -*- lexical-binding: t; -*-

(defun conv/text-input ()
  "pop up emacs window to edit text in then save to clipboard and close frame a la josh blais"
  (interactive)
  (let ((buf (get-buffer-create "*conv/text-input*")))
    (with-current-buffer buf
      (erase-buffer)
      (when (fboundp 'flyspell-mode)
        (flyspell-mode))
      (fset 'conv/text-input-finish
            (lambda ()
              (interactive)
              (clipboard-kill-region 1 (progn
                                         (end-of-buffer)
                                         (point-max)))
              (kill-buffer)
              (delete-frame)))
      (fset 'conv/text-input-never-mind
            (lambda ()
              (interactive)
              (erase-buffer)
              (kill-buffer)
              (delete-frame)))
      (keymap-local-set "C-c C-c" 'conv/text-input-finish)
      (keymap-local-set "C-c C-k" 'conv/text-input-never-mind)
      (setq mode-line-format " text input 😼 | C-c C-c to copy to clipboard and close | C-c C-k to erase and close"))
    (switch-to-buffer buf)))

(defun conv/package-upgrade ()
  "upgrades all the packages and asks you whether to upgrade for all"
  (interactive)
  (when (y-or-n-p (format "%s packages to upgrade. proceed?" (length (package--upgradeable-packages))))
    (dolist (pack (package--upgradeable-packages))
      (when (y-or-n-p (format "upgrade %s?" pack))
        (package-upgrade pack)
        (user-error "upgrade aborted :<")))))

(defun conv/save-buffers-kill-terminal ()
  "runs conv/save-buffers-kill-terminal hook and then runs save-buffers-kill-terminal"
  (interactive)
  (run-hooks 'conv/save-buffers-kill-terminal-hook)
  (save-buffers-kill-terminal))

(add-hook 'conv/save-buffers-kill-terminal-hook '(lambda ()
												   (setq conv/last-buffer (buffer-name))))

(defun conv/switch-to-last-buffer ()
  "switches to conv/last-buffer"
  (switch-to-buffer conv/last-buffer))

(keymap-global-set "C-x C-c" 'conv/save-buffers-kill-terminal)

(defun conv/fortune-cowsay ()
  "gets scratch buffer and interts the output from `fortune -c | cowsay -r'"
  (interactive)
  (switch-to-buffer (get-scratch-buffer-create))
  (insert (shell-command-to-string "fortune -c | cowsay -r")))
