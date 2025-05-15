#!/bin/zsh

# We do our own setup.
setopt no_global_rcs

# Variables
export DOTFILES_HOME="/opt/p/dotfiles"
export ZDOTDIR="${DOTFILES_HOME}/zsh"

function autoload-executables-in-dir() {
  local autoload_dir="$1"
  fpath=("${autoload_dir}" "${fpath[@]}")

  # Autoload all shell functions from in a given directory that have
  # the executable bit set. The executable bit is not necessary, but
  # gives you an easy way to stop the autoloading of a particular
  # shell function.
  for func in "${autoload_dir}"/*(N-.x:t); do
    autoload -Uz "$func"
  done
}

# Setup function and completion directories.
autoload-executables-in-dir "${ZDOTDIR}/completions"
autoload-executables-in-dir "${ZDOTDIR}/functions"
autoload-executables-in-dir "${ZDOTDIR}/iosource"
unfunction autoload-executables-in-dir
