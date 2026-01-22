#!/usr/bin/zsh
if [ -x $(which fortune 2>/dev/null || echo dne) -a -x $(which cowsay 2>/dev/null || echo dne) ] ; then
   emacsclient -c -a '' -e "(progn (toggle-frame-fullscreen) (select-frame-set-input-focus (selected-frame)) (or (and (boundp 'conv/last-buffer) (conv/switch-to-last-buffer)) (with-current-buffer (get-buffer-create \"*fortune*\") (insert (shell-command-to-string \"fortune -c | cowthink -f skeleton\"))) (switch-to-buffer \"*fortune*\")))"
   else
	   emacsclient -c -a '' -e "(progn (toggle-frame-fullscreen) (select-frame-set-input-focus (selected-frame)) (or (and (boundp 'conv/last-buffer) (conv/switch-to-last-buffer)) (dired-jump)))"
fi
# emacsclient -c -a '' -e "(progn (toggle-frame-fullscreen) (select-frame-set-input-focus (selected-frame)))"
