#!/bin/sh
pathmunge () {
if ! echo $PATH | grep -Eq "(^|:)$1($|:)" ; then
   if [ "$2" = "after" ] ; then
      PATH=$PATH:$1
   else
      PATH=$1:$PATH
   fi
fi
}
path_remove ()  {
    export PATH=`echo -n $PATH | awk -v RS=: -v ORS=: '$0 != "'$1'"' | sed 's/:$//'`;
}
# remove /usr/local/bin and readd before /usr/bin

path_remove "/usr/local/bin"
pathmunge "/usr/local/bin"
pathmunge "/usr/local/sbin"
pathmunge "$HOME/bin"

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
