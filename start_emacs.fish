#!/usr/bin/env fish
argparse -x c,F c/client F/fullscreen f/floating e/eval= s/select_frame -- $argv

#TODO: make select frame / auto maximize functionality

if set -q _flag_c
		set emacs_command "emacsclient -c -a ''"
else
		set emacs_command "emacs"
end

if set -q _flag_F
		set -a emacs_command "-fs"
end

if set -q _flag_f
		set -a emacs_command "-F '((name . \"floating\"))'" # this requires some work on your window manager's side
end

if set -q _flag_e
		set -a emacs_command "-e $_flag_e"
end

eval "$emacs_command"
