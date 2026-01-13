#!/bin/sh
if [ -x /usr/bin/fortune -a -x /usr/bin/cowsay ] ; then
   emacsclient -c -a '' -e "(progn (toggle-frame-fullscreen) (select-frame-set-input-focus (selected-frame)) (or (and (boundp 'conv/last-buffer) (conv/switch-to-last-buffer)) (with-current-buffer (get-buffer-create \"*initial*\") (insert (shell-command-to-string \"fortune -c | cowsay -f skeleton\"))) (switch-to-buffer \"*initial*\")))"
   else
	   emacsclient -c -a '' -e "(progn (toggle-frame-fullscreen) (select-frame-set-input-focus (selected-frame)) (or (and (boundp 'conv/last-buffer) (conv/switch-to-last-buffer)) (dired-jump)))"
fi
