#!/bin/sh
export GEM_HOME="$HOME/.gem"
# export WMII_CONFPATH="$HOME/.wmii"
export PATH=".:$HOME/bin:/usr/local/bin:/usr/local/sbin:$PATH"
export TERM="xterm-256color"
export ALTERNATE_EDITOR="emacs"
export EDITOR="emacsclient -a emacs"
export VISUAL="emacsclient"
export WORKON_HOME="$HOME/.virtualenvs"
export PROJECT_HOME="$HOME/prog"
export PIP_DOWNLOAD_CACHE="$HOME/.pip/cache"
export PIP_REQUIRE_VIRTUALENV=true
export PIP_RESPECT_VIRTUALENV=true
export VIRTUALENV_USE_DISTRIBUTE=true
export VIRTUAL_ENV_DISABLE_PROMPT=1
# Set CLICOLOR if you want Ansi Colors in iTerm2
export CLICOLOR=1
# source virtualenvwrapper.sh
