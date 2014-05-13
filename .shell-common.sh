#!/bin/sh
export GEM_HOME="$HOME/.gem"
# export WMII_CONFPATH="$HOME/.wmii"
export PATH=".:$PATH:$HOME/bin"
export TERM="xterm-256color"
export ALTERNATE_EDITOR="emacs"
export EDITOR="emacsclient -a emacs"
export VISUAL="emacsclient"
export WORKON_HOME="$HOME/.virtualenvs"
export PIP_DOWNLOAD_CACHE="$HOME/.pip/cache"
export PIP_REQUIRE_VIRTUALENV=true
export PIP_RESPECT_VIRTUALENV=true
export VIRTUALENV_USE_DISTRIBUTE=true

# Set CLICOLOR if you want Ansi Colors in iTerm2
export CLICOLOR=1
# source virtualenvwrapper.sh
