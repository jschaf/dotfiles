#!/bin/zsh

# We need to tell ZSH that configs are in ZDOTDIR.  This is the easiest way to
# make sure it always gets loaded.
export ZDOTDIR=$HOME/.zsh
if [[ -f $ZDOTDIR/.zshenv ]]; then
  . $ZDOTDIR/.zshenv
fi
