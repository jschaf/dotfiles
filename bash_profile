#!/bin/bash
echo "loading .bash_profile"
JOE_BASH_PROFILE_WAS_LOADED='yes'

pathmunge () {
    if ! echo $PATH | grep -Eq "(^|:)$1($|:)" ; then
        if [ "$2" = "after" ] ; then
            PATH=$PATH:$1
        else
            PATH=$1:$PATH
        fi
    fi
# Log bash startup information to this file.  Used mainly to debug why files
# aren't sourced and $PATH isn't updated.
INIT_LOG_FILE="${HOME}/.bash-init-log"

function setup-init-log() {
  if [[ -f "${INIT_LOG_FILE}" ]]; then
      touch "${INIT_LOG_FILE}"
  fi

  # Clear the file
  echo -n '' > "${INIT_LOG_FILE}"
}
path_remove ()  {
    PATH=`echo -n $PATH | awk -v RS=: -v ORS=: '$0 != "'$1'"' | sed 's/:$//'`;
function include () {
  [[ -e "$1" ]] && source "$1"
}

# like a stack, so the last entry is the first directory searched for
# executables.
# pathmunge "/usr/share/texmf-dist/scripts/texlive"

pathmunge "/usr/local/bin"
pathmunge "/usr/local/sbin"

# pathmunge "$HOME/.rvm/bin"
# pathmunge "$HOME/.cabal/bin"
# pathmunge "$HOME/.cargo/bin"
# pathmunge "$HOME/.cask/bin"
# pathmunge "$HOME/.local/bin"

pathmunge "$HOME/homebrew/bin"

# Add coreutils to path
command -v brew >/dev/null 2>&1 && [ -d "$(brew --prefix coreutils)/libexec/gnubin" ] && \
    pathmunge "$(brew --prefix coreutils)/libexec/gnubin"

pathmunge "$HOME/bin"
pathmunge "$HOME/bin-system"

[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx

if [[ $(uname) == Darwin ]]; then
    if [ -f "${HOME}/.gnupg/gpg-agent-info" ]; then
        . "${HOME}/.gnupg/gpg-agent-info"
        export GPG_AGENT_INFO
        export SSH_AUTH_SOCK
    fi
fi

export GPG_TTY=$(tty)
