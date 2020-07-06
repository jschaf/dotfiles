#!/bin/zsh

export ZDOTDIR="${HOME}/.zsh"

# These must be defined here because we need them to autoload the
# functions below.
export ZSH_DOTFILES="${DOTFILES_HOME}/zsh"
export ZSH_WORK_DOTFILES="${HOME}/.dotfiles-work/zsh"

# Uncomment to profile ZSH startup, or use the `profile-zsh` function.
# ZSH_PROFILE_RC=1

function autoload-executables-in-dir() {
  local autoload_dir="$1"
  fpath+="${autoload_dir}"

  # Autoload all shell functions from in a given directory that have
  # the executable bit set.  The executable bit is not necessary, but
  # gives you an easy way to stop the autoloading of a particular
  # shell function.
  for func in ${autoload_dir}/*(N-.x:t); do
    autoload -Uz $func;
  done
}

# Setup function and completion directories.
autoload-executables-in-dir "${ZSH_DOTFILES}/completions"
autoload-executables-in-dir "${ZSH_DOTFILES}/functions"
autoload-executables-in-dir "${ZSH_DOTFILES}/iosource"

function source-if-exists() {
  [[ -e "$1" ]] && source "$1"
}

source-if-exists "${ZSH_WORK_DOTFILES}/work-env.zsh"
source-if-exists "${ZSH_WORK_DOTFILES}/host-env.zsh"

