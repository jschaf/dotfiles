#!/bin/zsh

is-profiling-zsh && zsup-beginning-of-startup-file

# Load the config shared between bash and zsh.
source $HOME/.profile

# Path Setup
#
# ZSH ties the $path array variable to the $PATH environmental variable via
#`typeset -T`.  We can make the $path array only have unique entries with
#`typeset -U`
typeset -U path PATH cdpath CDPATH fpath FPATH manpath MANPATH

# If we have don't have a display and we're on TTY1.
if [[ -z "$DISPLAY" ]] && [[ -n "$XDG_VTNR" ]] && [[ "$XDG_VTNR" -eq 1 ]]; then
  exec startx ~/.config/X11/xinitrc
fi

is-profiling-zsh && zsup-end-of-startup-file
