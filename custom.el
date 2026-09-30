;; -*- lexical-binding: t; -*-

(setq-local time/custom (current-time))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight light :height 145 :width normal :foundry "JB" :family "Sarasa Mono Slab J" :foreground "black"))))
 '(cursor ((t (:background "black"))))
 '(eshell-prompt ((t (:inherit modus-themes-prompt :foreground "red")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(package-selected-packages
	 '(constants conv counsel dired-sidebar dockerfile-mode doom-modeline
							 doom-themes fireplace fish-mode ivy-rich lsp-mode magit
							 multiple-cursors pink-bliss-uwu-theme rust-mode tempel
							 tldr typescript-mode unicode-emoticons vterm))
 '(warning-suppress-types '((frameset))))

;; ;; Source - https://stackoverflow.com/q/69232418
;; ;; Posted by 3rdRealm
;; ;; Retrieved 2026-02-19, License - CC BY-SA 4.0

(defconst jetbrains-ligature-mode--ligatures
  '("-->" "<!--" "->>" "<<-" "->" "<-" "=/="
    "<=>" "==" "!=" "<=" ">=" "!==" "===" "=>" "<->" "==>"
;;     "|||" "&&&" "&=" "++" "--"
;;     "|||>" "<|||" ">>" "<<" "::=" ":?>" ":?" "/=" "?:" "?." "::"
;;     "+++" "??" "###" "##" ":::" "####" ".?" "?=" "=!=" "<|>"
;;     "<:" ":<" ":>" ">:" "<>" "/==" ".=" ".-" "__"
;;      "<-<" "<<<" ">>>" "<=<" "<<=" "<==" "<==>" "=>>"
;;     ">=>" ">>=" ">>-" ">-" "<~>" "-<" "-<<" "=<<" "---" "<-|"
;;     "<=|" "/\\" "\\/" "|=>" "|~>" "<~~" "<~" "~~" "~~>" "~>"
;;     "<$>" "<$" "$>" "<+>" "<+" "+>" "<*>" "<*" "*>" "</>" "</" "/>"
;;      "..<" "~=" "~-" "-~" "~@" "^=" "-|" "_|_" "|-" "||-"
;;     "#?" "#_" "#_(" "#:" "#!" "#="
    ))

(sort jetbrains-ligature-mode--ligatures (lambda (x y) (> (length x) (length y))))

(dolist (pat jetbrains-ligature-mode--ligatures)
  (set-char-table-range composition-function-table
												(aref pat 0)
												(nconc (char-table-range composition-function-table (aref pat 0))
                               (list (vector (regexp-quote pat)
																						 0
																						 'compose-gstring-for-graphic)))))

(add-to-list 'emacs-init-times `("custom" . ,(float-time (time-subtract (current-time) time/custom))))
