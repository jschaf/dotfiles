#!/bin/sh
# ~/.profile is read by the display manager.

export _SOURCED_PROFILE='yes'

# General config
export DOTFILES_HOME="/p/dotfiles"
export DOTFILES_WORK="/p/dotfiles-work"
export XDG_CONFIG_HOME="${HOME}/.config"

export OS_TYPE
OS_TYPE="$(uname -s)"

export DISTRO_TYPE='unknown'
if [ -r /etc/arch-release ]; then DISTRO_TYPE='arch'; fi
if [ -r /etc/debian_version ]; then DISTRO_TYPE='debian'; fi

# Set terminal property (used by msgid-chooser).
export COLORTERM="yes"
export CLICOLOR=1
export PAGER=${PAGER:-less}

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

# For zsh compatibility with bash.
if [ -z "$HOSTNAME" ]; then
  export HOSTNAME=$HOST
fi

if [ ! -d "${HOME}/.terminfo" ]; then
  print "Adding xterm-24bit as terminal description."
  /usr/bin/tic -x -o "${HOME}/.terminfo" "${DOTFILES_HOME}/terminfo/xterm-24bit.terminfo"
fi
export TERM=xterm-24bit

# Set the NPM auth token if it exists.
npm_auth_token_file="$HOME/.config/npm/npm-auth-token"
export NPM_AUTH_TOKEN="NOT_INITIALIZED_FROM_FILE"
if [ -f "$npm_auth_token_file" ]; then
  NPM_AUTH_TOKEN="$(cat "${HOME}/.config/npm/npm-auth-token")"
fi
unset npm_auth_token_file


# Mac specific
if [ "${OS_TYPE}" = 'Darwin' ]; then
  github_personal_token_file="$HOME/.config/github/personal-token"
  export HOMEBREW_GITHUB_API_TOKEN='NOT_INITIALIZED_YET'
  if [ -f "$github_personal_token_file" ]; then
    HOMEBREW_GITHUB_API_TOKEN="$(cat "${github_personal_token_file}")"
  fi
  unset github_personal_token_file

  # Java
  if [ -z "${JAVA_HOME}" ]; then
    # https://stackoverflow.com/questions/21964709
    export JAVA_HOME
    JAVA_HOME=$(/usr/libexec/java_home -v 1.8)
  fi
fi

# prepend_to_path adds args the beginning of the path
# https://unix.stackexchange.com/a/4973/179300
prepend_to_path() {
  for d; do
    # canonicalize symbolic links
    d=$({ cd -- "$d" && { pwd -P || pwd; }; } 2>/dev/null)
    # skip nonexistent directory
    if [ -z "$d" ]; then continue; fi
    case ":$PATH:" in
    # skip if path already in entry
    *":$d:"*) : ;;
    *) PATH=$d:$PATH ;;
    esac
  done
}

# List in reverse order so that the last entry has the highest priority.
prepend_to_path \
  "/usr/share/texmf-dist/scripts/texlive" \
  "/usr/local/bin" \
  "${GEM_HOME}/bin" \
  "${HOME}/.yarn/bin" \
  "${HOME}/.cargo/bin" \
  "${HOME}/.cask/bin" \
  "${GOPATH}/bin" \
  "${DOTFILES_HOME}/zsh/iosource" \
  "${HOME}/bin" \
  "${DOTFILES_HOME}/bin" \
  "${DOTFILES_WORK}/bin"
unset -f prepend_to_path

# clean_path removes directories that don't exist and removes unused
# directories like /usr/local/games.
# https://stackoverflow.com/a/53223471/30900
clean_path() {
  # IFS characters are stripped when using `read`
  oldIFS="$IFS"
  IFS=''
  # Beware pipes. They imply subshells. The usual alternative is to use
  # process substitution, but it won't work with dash, so I used file
  # descriptor redirections instead.
  {
    PATH="$(echo "$PATH" | {
      P=""
      # read -d is supported by dash.
      # shellcheck disable=SC2039
      while read -rd: dir; do
        if [ "$dir" = '/usr/games' ] || [ "$dir" = '/usr/local/games' ]; then
          continue
        elif [ -d "$dir" ]; then
          P="$P:$dir"
        fi
      done
      # Remove leading colon. We added a colon for every directory.
      echo "${P#?}"
    })"
  } 3>&1
  IFS="$oldIFS"
}
clean_path && unset -f clean_path
