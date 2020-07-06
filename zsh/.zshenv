#!/bin/zsh


# Remove since duplicated in zprofile.
export OS_TYPE
OS_TYPE="$(uname -s)"
function is-linux() { [[ "${OS_TYPE}" == "Linux" ]]; }
function is-darwin() { [[ "${OS_TYPE}" == "Darwin" ]]; }
function is-macos() { [[ "${OS_TYPE}" == "Darwin" ]]; }
function is-freebsd() { [[ "${OS_TYPE}" == "FreeBSD" ]]; }

export DISTRO_TYPE='unknown'
if [[ -r /etc/arch-release ]]; then DISTRO_TYPE='arch'; fi
if [[ -r /etc/debian_version ]]; then DISTRO_TYPE='debian'; fi
function is-arch-distro() { [[ "${DISTRO_TYPE}" == 'arch' ]]; }
function is-debian-distro() { [[ "${DISTRO_TYPE}" == 'debian' ]]; }

# These must be defined here because we need them to autoload the
# functions below.
export ZSH_DOTFILES="${DOTFILES_HOME}/zsh"
export ZSH_WORK_DOTFILES="${DOTFILES_WORK}/zsh"

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
  [[ -e "$1" ]] && source "$1"
}

source-if-exists "${ZSH_WORK_DOTFILES}/work-env.zsh"
source-if-exists "${ZSH_WORK_DOTFILES}/host-env.zsh"

