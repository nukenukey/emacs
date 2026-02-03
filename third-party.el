;; -*- lexical-binding: t; -*-

(use-package fish-mode
  :commands
  (fish-mode)
  :defer nil
  :ensure t)

(use-package company
  :commands
  (company-mode)
  :defer nil
  :ensure t
  :bind
  ("C-x j q" . 'company-mode))

(use-package tldr
  :commands
  (tldr)
  :defer t
  :ensure t)

(use-package magit
  :defer nil
  :ensure t)

(use-package vterm
  :commands
  (vterm)
  :bind
  ("C-x j v" . 'vterm)
  :defer t
  :ensure t
  :config
  (setq vterm-shell "/usr/bin/fish")
  (dolist (key '("M-:"
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
    (unbind-key key vterm-mode-map)))

(use-package swiper
  :defer t
  :ensure t
  :bind
  ("C-s" . 'swiper)
  ("C-S-s" . 'swiper-thing-at-point))

(use-package counsel
  :defer t
  :ensure t)

(use-package ivy-rich
  :defer t
  :ensure t)

(use-package ivy
  :defer nil ;; I will want ivy always
  :ensure t
  :bind
  ("C-x b" . 'counsel-ibuffer)
  :init
  (setq ivy-initial-inputs-alist nil) ;; no leading ^ pls
  :config
  (ivy-mode)
  (ivy-rich-mode)
  (counsel-mode))

(use-package multiple-cursors
  :defer nil ;; I want this always available
  :ensure t
  :bind
  ("C-x m p" . 'mc/mark-previous-lines)
  ("C-x m C-p" . 'mc/mark-previous-like-this)
  ("C-x m n" . 'mc/mark-next-lines)
  ("C-x m C-n" . 'mc/mark-next-like-this)
  ("C-x m !" . 'mc/mark-all-like-this)
  ("C-x m r" . 'mc/mark-all-in-region)
  ("C-x m a" . 'mc/edit-beginnings-of-lines)
  ("C-x m e" . 'mc/edit-ends-of-lines)
  :config
  (add-hook 'multiple-cursors-mode-hook (lambda ()
										  (if multiple-cursors-mode
											  (setq cursor-type t)
											(setq cursor-type 'bar))))
  (unbind-key "C-x m" global-map))

(use-package fireplace
  :defer nil ;; make sure this is installed
  :ensure t)

(use-package doom-modeline
  :defer nil
  :ensure t
  :init
  
  (setq doom-modeline-icon nil)
  (unless conv/sway
	(setq	 doom-modeline-time t
			 doom-modeline-time-analogue-clock t
			 doom-modeline-time-clock-size 11
			 display-time-format "%H:%M %a %b %d"
			 display-time-default-load-average nil
			 doom-modeline-battery t)
	(display-battery-mode)
	(display-time))
  :config
  (doom-modeline-mode))
