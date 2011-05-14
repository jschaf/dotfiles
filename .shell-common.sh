#!/bin/sh
export GEM_HOME="$HOME/.gem"
export PATH=".:$HOME/bin:$HOME/.cabal/bin:$HOME/.gem/bin:$PATH"
export TERM="screen-256color"
export EDITOR="emacs"
export WORKON_HOME="$HOME/.virtualenvs"
export PIP_DOWNLOAD_CACHE="$HOME/.pip/cache"
export PIP_REQUIRE_VIRTUALENV=true
export PIP_RESPECT_VIRTUALENV=true
export VIRTUALENV_USE_DISTRIBUTE=true
source virtualenvwrapper.sh
