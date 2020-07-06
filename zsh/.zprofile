#!/bin/zsh

# ZSH config
export ZDOTDIR="${HOME}/.zsh"

# General Settings
export DOTFILES_HOME="/p/dotfiles"
export DOTFILES_WORK="/p/dotfiles-work"
export XDG_CONFIG_HOME="${HOME}/.config"

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

# Returns 0 if the current terminal is a TTY.
#
# TTY is ambiguous, but I'm using it to mean where at a framebuffer terminal
# that doesn't have UTF-8 and is limited to 8 colors.
function is-tty() { [[ $(tty) == /dev/tty[0-9] ]]; }

# Set terminal property (used by msgid-chooser).
export COLORTERM="yes"
export CLICOLOR=1
export PAGER=${PAGER:-less}

if [[ ! -d "${HOME}/.terminfo" ]]; then
  print "Adding xterm-24bit as terminal description."
  /usr/bin/tic -x -o "${HOME}/.terminfo" "${DOTFILES_HOME}/terminfo/xterm-24bit.terminfo"
fi
export TERM=xterm-24bit

if [[ -z $HOSTNAME ]]; then
  # For zsh compatibility with bash.
  export HOSTNAME=$HOST
fi

npm_auth_token_file="$HOME/.config/npm/npm-auth-token"
export NPM_AUTH_TOKEN="NOT_INITIALIZED_FROM_FILE"
if [[ -f "$npm_auth_token_file" ]]; then
  NPM_AUTH_TOKEN="$(<"${HOME}/.config/npm/npm-auth-token")"
fi
unset npm_auth_token_file

export PERSONAL_DICTIONARY=${HOME}/.config/personalDictionary/words.txt

# Editors
export ALTERNATE_EDITOR="emacs"
export EDITOR="emacsclient -nw"
export VISUAL="emacsclient --alternate-editor=emacs"
export FPP_EDITOR="emacsclient --no-wait -a emacs"

# Go setup
export GOPATH='/go'

# Ruby setup
export GEM_HOME="$HOME/.gems"

# Rust setup
export RUST_SRC_PATH="${HOME}/.multirust/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src"

typeset -U path # force unique; the path array is tied to $PATH (typeset -t).
# add_to_path adds an entry to the path if the directory exists. Moves the entry
# to the front if the the entry is already present in the path array.
function add_to_path() {
  p="$1"
  if [[ -d "$p" ]]; then
    path[1,0]="$p" # https://unix.stackexchange.com/a/62599
  fi
}

# Mac specific
if [[ "${OS_TYPE}" == 'Darwin' ]]; then
  github_personal_token_file="$HOME/.config/github/personal-token"
  export HOMEBREW_GITHUB_API_TOKEN='NOT_INITIALIZED_YET'
  if [[ -f "$github_personal_token_file" ]]; then
    HOMEBREW_GITHUB_API_TOKEN="$(<"${github_personal_token_file}")"
  fi
  unset github_personal_token_file

  add_to_path "/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/bin"

  # Java
  if [[ -z "${JAVA_HOME}" ]]; then
    # https://stackoverflow.com/questions/21964709/how-to-set-or-change-the-default-java-jdk-version-on-os-x
    export JAVA_HOME
    JAVA_HOME=$(/usr/libexec/java_home -v 1.8)
  fi
fi

# NOTE: on MacOS, we'll read /etc/zprofile after this which runs path_helper and
# prepends the contents of /etc/paths and /etc/paths.d/* to $PATH effectively
# overriding our config.
add_to_path "${DOTFILES_WORK}/bin"
add_to_path "${DOTFILES_HOME}/bin"
add_to_path "${HOME}/bin"
add_to_path "${DOTFILES_HOME}/zsh/iosource"
add_to_path "${GOPATH}/bin"
add_to_path "${HOME}/.cask/bin"
add_to_path "${HOME}/.cargo/bin"
add_to_path "${HOME}/.yarn/bin"
add_to_path "${GEM_HOME}/bin"
add_to_path "/usr/local/bin"
add_to_path "/usr/share/texmf-dist/scripts/texlive"
unfunction add_to_path

rehash

OLD_MANPATH="$MANPATH"
MANPATH+=":/usr/man"
MANPATH+=":/usr/local/man"
MANPATH+=":$OLD_MANPATH"

# Remove unnecessary elements from fpath. Keeps commented out lines.
typeset -a fpath_remove=(
  /usr/local/share/zsh/site-functions
  /usr/share/zsh/vendor-functions
  # /usr/share/zsh/vendor-completions
  /usr/share/zsh/functions/Calendar
  /usr/share/zsh/functions/Chpwd
  # /usr/share/zsh/functions/Completion
  /usr/share/zsh/functions/Completion/AIX
  /usr/share/zsh/functions/Completion/BSD
  # /usr/share/zsh/functions/Completion/Base
  /usr/share/zsh/functions/Completion/Cygwin
  /usr/share/zsh/functions/Completion/Darwin
  # /usr/share/zsh/functions/Completion/Debian
  # /usr/share/zsh/functions/Completion/Linux
  /usr/share/zsh/functions/Completion/Mandriva
  /usr/share/zsh/functions/Completion/Redhat
  /usr/share/zsh/functions/Completion/Solaris
  /usr/share/zsh/functions/Completion/Unix
  # /usr/share/zsh/functions/Completion/X
  # /usr/share/zsh/functions/Completion/Zsh
  /usr/share/zsh/functions/Completion/openSUSE
  /usr/share/zsh/functions/Exceptions
  /usr/share/zsh/functions/MIME
  # /usr/share/zsh/functions/Math
  # /usr/share/zsh/functions/Misc
  /usr/share/zsh/functions/Newuser
  /usr/share/zsh/functions/Prompts
  /usr/share/zsh/functions/TCP
  /usr/share/zsh/functions/VCS_Info
  /usr/share/zsh/functions/VCS_Info/Backends
  /usr/share/zsh/functions/Zftp
  # /usr/share/zsh/functions/Zle
)
# Remove entries in fpath that are part of the array fpath_remove.
# https://stackoverflow.com/a/52188874
fpath=(
  ${fpath:|fpath_remove}
)
unset fpath_remove

# Load common functions directly to avoid searching fpath.
autoload /usr/share/zsh/functions/Completion/compinit
autoload /usr/share/zsh/functions/Prompts/promptinit
autoload /usr/share/zsh/functions/Misc/add-zsh-hook
autoload /usr/share/zsh/functions/Misc/add-zle-hook-widget
autoload /p/dotfiles/zsh/functions/external-command-exists
autoload /p/dotfiles/zsh/functions/command-exists
autoload /p/dotfiles/zsh/prompts/prompt_pure_setup
autoload /p/dotfiles/zsh/functions/salias

# http://zsh.sourceforge.net/Doc/Release/Parameters.html
# $'STR' expands escape sequences: http://zsh.sourceforge.net/Guide/zshguide05.html#l115
export TIMEFMT=$'\nreal\t%*E\nuser\t%*U\nsys\t%*S\nmaxmem\t%M MB\nfaults\t%F'
