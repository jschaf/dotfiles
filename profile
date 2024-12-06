#!/bin/sh
# ~/.profile is read by the display manager on login.

export _SOURCED_PROFILE='yes'

export OS_TYPE
OS_TYPE="$(uname -s)"

# General config
export DOTFILES_HOME="/opt/p/dotfiles"
export XDG_CONFIG_HOME="${HOME}/.config"

export DISTRO_TYPE='unknown'
if [ -r /etc/debian_version ]; then
  DISTRO_TYPE='debian'
elif [ -r /etc/arch-release ]; then
  DISTRO_TYPE='arch'
fi

# Set terminal property (used by msgid-chooser).
export COLORTERM="yes"
export CLICOLOR=1
export PAGER="${PAGER:-less}"

# Editors
export ALTERNATE_EDITOR="emacs"
export EDITOR="emacsclient -nw"
export VISUAL="emacsclient --alternate-editor=emacs"
export DOOMDIR="${DOTFILES_HOME}/doom"
