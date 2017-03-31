#!/bin/zsh

# Disable autocompletion setup by Google /etc/zshrc
# google_zsh_flysolo='I march to my own drum'

echo zsh env
source "${HOME}/.shell-common.sh"
echo common

function contains() {
  string="$1"
  substring="$2"
  if test "${string#*$substring}" != "$string"
  then
    return 0    # $substring is in $string
  else
    return 1    # $substring is not in $string
  fi
}

function add-to-colon-separated-env-var() {
  local envKey="$1"
  local envValue="${(P)1}"
  local pathToAdd="$2"
  local insertLocation="$3"

  if ! contains "${envValue}" "${pathToAdd}" ; then
    if [ "${insertLocation}" = "after" ] ; then
      eval "${envKey}=${envValue}:${pathToAdd}"
    else
      eval "${envKey}=${pathToAdd}:${envValue}"
    fi
  fi
}

function add-to-path () {
  add-to-colon-separated-env-var PATH "$1" "$2"
}

function add-to-path-if-exists() {
  if [[ -d "$1" ]]; then
    add-to-path "$1" "$2"
  fi
}

# Path Setup
#
# View adding paths without a second arg of 'after' like a stack, so the last
# entry is the first directory searched for executables.
add-to-path-if-exists "/usr/share/texmf-dist/scripts/texlive"
add-to-path-if-exists "${HOME}/.cask/bin"
add-to-path "/usr/local/bin"
add-to-path-if-exists "/usr/local/sbin"
add-to-path-if-exists "$HOME/homebrew/bin"

add-to-path-if-exists "$HOME/prog/bin"
add-to-path "$HOME/bin"
add-to-path "$HOME/bin-system"

echo end zshenv
