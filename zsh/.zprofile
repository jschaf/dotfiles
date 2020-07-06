#!/bin/zsh

# General Settings
export DOTFILES_HOME="/p/dotfiles"
export XDG_CONFIG_HOME="${HOME}/.config"
export OS_TYPE
OS_TYPE="$(uname -s)"
# Set terminal property (used by msgid-chooser).
export COLORTERM="yes"
export CLICOLOR=1
export PAGER=${PAGER:-less}

if [[ ! -d "${HOME}/.terminfo" ]]; then
  print "Adding xterm-24bit as terminal description."
  /usr/bin/tic -x -o "${HOME}/.terminfo"  "${DOTFILES_HOME}/terminfo/xterm-24bit.terminfo"
fi
export TERM=xterm-24bit

export DIFF="colordiff -u "

if [[ -z $HOSTNAME ]]; then
  # For zsh compatibility with bash.
  export HOSTNAME=$HOST
fi

npm_auth_token_file="$HOME/.config/npm/npm-auth-token"
export NPM_AUTH_TOKEN="NOT_INITIALIZED_FROM_FILE"
if [[ -f "$npm_auth_token_file" ]]; then
  NPM_AUTH_TOKEN="$(< "${HOME}/.config/npm/npm-auth-token")"
fi
unset npm_auth_token_file

# Dotfiles
export DOTFILES_DIR=${HOME}/.dotfiles
export DOTFILES_WORK_DIR=${HOME}/.dotfiles-work

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

# Mac specific
if [[ "${OS_TYPE}" == 'Darwin' ]]; then

  github_personal_token_file="$HOME/.config/github/personal-token"
  export HOMEBREW_GITHUB_API_TOKEN='NOT_INITIALIZED_YET'
  if [[ -f "$github_personal_token_file" ]]; then
    HOMEBREW_GITHUB_API_TOKEN="$(< ${github_personal_token_file})"
  fi
  unset github_personal_token_file

  PATH+=":/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/bin"

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
OLD_PATH="$PATH"
export PATH=""
function add_to_path_if_exists() {
  p="$1"
  if [[ -d "$p" ]]; then
    PATH+=":$p"
  fi
}
add_to_path_if_exists "${DOTFILES_WORK}/bin"
add_to_path_if_exists "${DOTFILES_HOME}/bin"
add_to_path_if_exists "${HOME}/bin"
add_to_path_if_exists "${DOTFILES_HOME}/zsh/iosource"
add_to_path_if_exists "${GOPATH}/bin"
add_to_path_if_exists "${HOME}/.cask/bin"
add_to_path_if_exists "${HOME}/.cargo/bin"
add_to_path_if_exists "${HOME}/.yarn/bin"
# Setup Ruby and Gem so we install packages without root.
add_to_path_if_exists "${GEM_HOME}/bin"
add_to_path_if_exists "/usr/local/bin"
add_to_path_if_exists "/usr/share/texmf-dist/scripts/texlive"
PATH+=":$OLD_PATH"
unfunction add_to_path_if_exists

rehash

OLD_MANPATH="$MANPATH"
MANPATH+=":/usr/man"
MANPATH+=":/usr/local/man"
MANPATH+=":$OLD_MANPATH"

# http://zsh.sourceforge.net/Doc/Release/Parameters.html
# $'STR' expands escape sequences: http://zsh.sourceforge.net/Guide/zshguide05.html#l115
export TIMEFMT=$'\nreal\t%*E\nuser\t%*U\nsys\t%*S\nmaxmem\t%M MB\nfaults\t%F'

