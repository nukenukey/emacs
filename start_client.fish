#!/usr/bin/env fish
argparse -x c,F c/client F/fullscreen f/floating s/select_frame e/eval= -- $argv ; or return

if set -q _flag_c
		set emacs_command "emacsclient -c -a '' "
else
		set emacs_command "emacs "
end

if set -q _flag_F
		set -a emacs_command "-fs "
end

if set -q _flag_f
		set -a emacs_command "-F \"\\\'((name . \"floating\"))\"" # this requires some work on your window manager's side
end

if set -q _flag_s
		set -a emacs_command "--eval=\"(progn (toggle-frame-fullscreen) (select-frame-set-input-focus (selected-frame)) $_flag_e)\""
end

eval "$emacs_command"
