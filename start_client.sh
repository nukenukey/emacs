#!/usr/bin/zsh
if [ ! -z "$COWSAY_CMD" ] ; then
	emacsclient -c -a '' -e "(progn (toggle-frame-fullscreen) (select-frame-set-input-focus (selected-frame)) (or (and (boundp 'conv/last-buffer) (conv/switch-to-last-buffer)) (with-current-buffer (get-buffer-create \"*fortune*\") (insert (shell-command-to-string \"${COWSAY_CMD}\"))) (switch-to-buffer \"*fortune*\")))"
else
	emacsclient -c -a '' -e "(progn (toggle-frame-fullscreen) (select-frame-set-input-focus (selected-frame)) (or (and (boundp 'conv/last-buffer) (conv/switch-to-last-buffer)) (dired-jump)))"
fi
