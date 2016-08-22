#!/bin/sh

export LANG=en_US.UTF-8
# Set CLICOLOR if you want Ansi Colors in iTerm2
export CLICOLOR=1
export TERM="xterm-256color"
export ALTERNATE_EDITOR="emacs"
export EDITOR="emacsclient -a emacs"
export VISUAL="emacsclient -a emacs"
export FPP_EDITOR="emacsclient --no-wait -a emacs"
export WORKON_HOME="$HOME/.dotfiles/home/.virtualenvs"
export PROJECT_HOME="$HOME/prog"
export DOTFILES_HOME="${HOME}/.dotfiles"

# NodeJS and NPM setup.
export NPM_PACKAGES="${HOME}/.npm-packages"
export NODE_PATH="${NPM_PACKAGES}/lib/node_modules:${NODE_PATH}"
