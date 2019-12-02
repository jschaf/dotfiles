#!/bin/zsh

is-profiling-zsh && zsup-beginning-of-startup-file

# Load the config shared between bash and zsh.
if [[ "${LOADED_SH_PROFILE}" != 'yes' ]]; then
  # Might already be loaded by .zshenv
  source "${HOME}/.profile"
fi

# http://zsh.sourceforge.net/Doc/Release/Parameters.html
# $'STR' expands escape sequences: http://zsh.sourceforge.net/Guide/zshguide05.html#l115
export TIMEFMT=$'\nreal\t%*E\nuser\t%*U\nsys\t%*S\nmaxmem\t%M MB\nfaults\t%F'

is-profiling-zsh && zsup-end-of-startup-file
