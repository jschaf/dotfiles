#!/bin/zsh

# The display manager sources ~/.profile so we should never hit this.
# Keep it just in case.
if [[ "${_SOURCED_PROFILE}" != 'yes' ]]; then
  emulate sh -c ". ${HOME}/.profile"
fi

export ZDOTDIR="${DOTFILES_HOME}/zsh"
export ZSH_DOTFILES="${DOTFILES_HOME}/zsh"
export ZSH_WORK_DOTFILES="${DOTFILES_WORK}/zsh"

# Since the terminal emulator defaults to a non-login shell, we will rarely load
# .zprofile at login. Load it here as a backup.
if [[ "${_SOURCED_ZSH_ZPROFILE}" != 'yes' ]]; then
  source "${ZDOTDIR}/.zprofile"
fi

function is-linux() { [[ "${OS_TYPE}" == "Linux" ]]; }
function is-darwin() { [[ "${OS_TYPE}" == "Darwin" ]]; }
function is-macos() { [[ "${OS_TYPE}" == "Darwin" ]]; }
function is-freebsd() { [[ "${OS_TYPE}" == "FreeBSD" ]]; }

function is-debian-distro() { [[ "${DISTRO_TYPE}" == 'debian' ]]; }

# Returns 0 if the current terminal is a TTY.
#
# TTY is ambiguous, but I'm using it to mean where at a framebuffer terminal
# that doesn't have UTF-8 and is limited to 8 colors.
function is-tty() { [[ $(tty) == /dev/tty[0-9] ]]; }

function autoload-executables-in-dir() {
  local autoload_dir="$1"
  fpath=("${autoload_dir}" "${fpath[@]}")

  # Autoload all shell functions from in a given directory that have
  # the executable bit set.  The executable bit is not necessary, but
  # gives you an easy way to stop the autoloading of a particular
  # shell function.
  for func in "${autoload_dir}"/*(N-.x:t); do
    autoload -Uz "$func";
  done
}

# Setup function and completion directories.
autoload-executables-in-dir "${ZSH_DOTFILES}/completions"
autoload-executables-in-dir "${ZSH_DOTFILES}/functions"
autoload-executables-in-dir "${ZSH_DOTFILES}/iosource"

function source-if-exists() {
  # shellcheck disable=SC1090
  [[ -e "$1" ]] && source "$1"
}

source-if-exists "${ZSH_WORK_DOTFILES}/work-env.zsh"
source-if-exists "${ZSH_WORK_DOTFILES}/host-env.zsh"

