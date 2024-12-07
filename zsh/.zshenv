#!/bin/zsh

zmodload zsh/zprof

# We do our own setup.
setopt no_global_rcs

# Variables
export DOTFILES_HOME="/opt/p/dotfiles"
export ZDOTDIR="${DOTFILES_HOME}/zsh"
export ZSH_DOTFILES="${DOTFILES_HOME}/zsh"

# Since the terminal emulator defaults to a non-login shell, we will rarely load
# .zprofile at login. Load it here as a backup.
if [[ "${_SOURCED_ZSH_ZPROFILE}" != 'yes' ]]; then
  source "${ZDOTDIR}/.zprofile"
fi

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
autoload-executables-in-dir "${ZSH_DOTFILES}/completions"
autoload-executables-in-dir "${ZSH_DOTFILES}/functions"
autoload-executables-in-dir "${ZSH_DOTFILES}/iosource"
