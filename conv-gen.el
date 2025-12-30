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
