;; -*- lexical-binding: t; -*-

(use-package org-bullets
  :defer t
  :ensure t)

(use-package tldr
  :defer t
  :ensure t)

(use-package magit
  :defer t
  :ensure t
  :config
  (add-hook 'magit-mode-hook (lambda ()
							   (keymap-local-set "C-v" 'vterm))))

(use-package vterm
  :defer t
  :ensure t
  :config
  (setq vterm-shell "/usr/bin/fish")
  (dolist (keys '("M-:"
                  "M-!"
                  "M-&"
                  "M-<"
                  "M->"
				  "M-L"
				  "M-J"
				  "M-K"
				  "M-H"
				  "M-W"
				  "C-SPC"
                  "M-w"))
    (unbind-key keys vterm-mode-map)))

(use-package counsel
  :defer t
  :ensure t)

(use-package ivy-rich
  :defer t
  :ensure t)

(use-package ivy
  :defer nil ;; I will want ivy always
  :ensure t
  :init
  (setq ivy-initial-inputs-alist nil) ;; no leading ^ pls
  :config
  ;; (ivy-mode)
  (ivy-rich-mode)
  (counsel-mode)
  (keymap-global-set "C-x b" 'counsel-ibuffer))

(use-package multiple-cursors
  :defer nil ;; I want this always available
  :ensure t
  :config
  (add-hook 'multiple-cursors-mode-hook (lambda ()
										  (if multiple-cursors-mode
											  (setq cursor-type t)
											(setq cursor-type 'bar))))
  (keymap-global-set "C-." 'mc/mark-next-like-this)
  (keymap-global-set "C-," 'mc/mark-previous-like-this)
  (keymap-global-set "C->" 'mc/mark-all-like-this))

(use-package all-the-icons
  :defer nil
  :ensure t
  :if (display-graphic-p))

(use-package dashboard
  :defer t
  :ensure t
  :config
  (setq image-scaling-factor 1.0)
  (setq dashboard-banner-logo-title "home sweet emacs")
  (setq dashboard-center-content t)
  (setq dashboard-vertically-center-content t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-icon-type 'all-the-icons)
  (setq dashboard-items '((projects . 5)
                          (recents . 7)))
  (setq dashboard-item-shortcuts '((recents . "f")
                                   (projects . "j")))
  (setq dashboard-footer-messages
        '("success"
          "yey emacs :D"
          "wooo emacs :P"
          "happy hacking 🩷"
          "Emacs is where the heart is"))
  (bind-key "n" 'dashboard-next-line dashboard-mode-map)
  (bind-key "p" 'dashboard-previous-line dashboard-mode-map)
  (bind-key "e" 'eshell dashboard-mode-map)
  (bind-key "v" 'vterm dashboard-mode-map)
  (unbind-key "j" dashboard-mode-map)
  (unbind-key "k" dashboard-mode-map))

(use-package fireplace
  :defer nil ;; make sure this is installed
  :ensure t)

(use-package doom-modeline
  :defer nil
  :ensure t
  :config
  (setq doom-modeline-icon nil)
  (setq doom-modeline-time t)
  (setq doom-modeline-time-analogue-clock t)
  (setq doom-modeline-time-clock-size 11)
  (setq display-time-format "%H:%M %a %b %d")
  (setq display-time-default-load-average nil)
  (setq doom-modeline-battery t)
  (display-battery-mode)
  (display-time)
  (doom-modeline-mode))
