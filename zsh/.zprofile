#!/bin/zsh

if [[ "${_SOURCED_PROFILE}" != 'yes' ]]; then
  emulate sh -c "${HOME}/.profile"
fi

export _SOURCED_ZSH_ZPROFILE='yes'

# Force PATH to be unique; the path array is tied to $PATH (typeset -t).
# shellcheck disable=SC2034
typeset -U path

path+=(/opt/homebrew/bin)

# Load common functions by absolute path to avoid searching every entry in $fpath.
autoload -Uz "${DOTFILES_HOME}/zsh/functions/command-exists"
autoload -Uz "${DOTFILES_HOME}/zsh/prompts/prompt_pure_setup"

# Print more info for time command.
# https://unix.stackexchange.com/a/562651/179300
#
# wall-time:   2700ms
# user-time:   243ms
# kernel-time: 2260ms
# cpu-percent: 92%  (243ms + 2260ms)/2700ms
# max-memory:  36320kB
# page-faults: 291 major, 84820 minor
# input-ops:   0
# output-ops:  0
# ctx-switch:  3675 voluntary, 22249 involuntary
# shellcheck disable=SC2034
TIMEFMT=$'\n wall-time:   %mE \n user-time:   %mU\n kernel-time: %mS \n cpu-percent: %P  (%mU + %mS)/%mE \n max-memory:  %MkB \n page-faults: %F major, %R minor \n input-ops:   %I \n output-ops:  %O \n ctx-switch:  %w voluntary, %c involuntary\n'

# Hide homebrew hints
export HOMEBREW_NO_ENV_HINTS=1
