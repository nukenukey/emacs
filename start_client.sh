#!/bin/sh
if [ "$XDG_CURRENT_DESKTOP" != "sway" ] ; then
	SELECT_FRAME="(toggle-frame-fullscreen) (select-frame-set-input-focus (selected-frame))"
else
	SELECT_FRAME=""
fi

start_daemon () {
		emacs --daemon || { notify-send "emacs daemon could not be started" ; exit 1 }
}

restart_emacs () {
		echo "restarting emacs"
		pkill -f "emacs --daemon" || { notify-send "emacs could not be restarted" ; exit 1 }
}

start_client () {
		emacsclient -c -a '' -e "(progn ${SELECT_FRAME} (dired-jump)))" || { notify-send "emacsclient could not be started" ; exit 1}
		# emacsclient -c -a '' -e "(progn ${SELECT_FRAME} (or (and (boundp 'conv/last-buffer) (conv/switch-to-last-buffer)) (dired-jump)))"
}

start_client || { restart_emacs && start_client }
