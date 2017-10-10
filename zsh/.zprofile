#!/bin/zsh

is-profiling-zsh && zsup-beginning-of-startup-file

# Load the config shared between bash and zsh.
source $HOME/.profile

# If we have don't have a display and we're on TTY1.
if [[ -z "$DISPLAY" ]] && [[ -n "$XDG_VTNR" ]] && [[ "$XDG_VTNR" -eq 1 ]]; then
  exec startx
fi

is-profiling-zsh && zsup-end-of-startup-file
