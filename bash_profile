#!/bin/bash
echo "loading .bash_profile"
JOE_BASH_PROFILE_WAS_LOADED='yes'

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

function include () {
  [[ -e "$1" ]] && source "$1"
}

include "${HOME}/.shell-common.sh"

function add-to-path () {
  if ! echo $PATH | grep -Eq "(^|:)$1($|:)" ; then
      if [ "$2" = "after" ] ; then
          PATH=$PATH:$1
      else
        PATH=$1:$PATH
      fi
  fi
}

function add-to-path-if-exists() {
  if [[ -d "$1" ]]; then
      echo "Adding $1 to path"
      add-to-path "$1" "$2"
  fi
}

# like a stack, so the last entry is the first directory searched for
# executables.
# add-to-path "/usr/share/texmf-dist/scripts/texlive"
function setup-path() {
  add-to-path-if-exists "/usr/local/bin"
  add-to-path-if-exists "/usr/local/sbin"
  add-to-path-if-exists "$HOME/.rvm/bin"
  add-to-path-if-exists "$HOME/.cabal/bin"
  add-to-path-if-exists "$HOME/.cargo/bin"
  add-to-path-if-exists "$HOME/.cask/bin"
  add-to-path-if-exists "$HOME/.local/bin"
  add-to-path-if-exists "$HOME/homebrew/bin"
  add-to-path-if-exists "${NPM_PACKAGES}/bin"
  add-to-path "$HOME/bin"
  add-to-path "$HOME/bin-system"
}

# Add coreutils to path
command -v brew >/dev/null 2>&1 && [ -d "$(brew --prefix coreutils)/libexec/gnubin" ] && \


export GPG_TTY=$(tty)
