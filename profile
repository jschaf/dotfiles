#!/bin/sh
# ~/.profile is read by the display manager on login.

export _SOURCED_PROFILE='yes'

export OS_TYPE
OS_TYPE="$(uname -s)"

# General config
export DOTFILES_HOME="/p/dots"
export DOTFILES_WORK="/p/dotsw"
if [ "$OS_TYPE" = 'Darwin' ]; then
  DOTFILES_HOME="/opt${DOTFILES_HOME}"
  DOTFILES_WORK="/opt${DOTFILES_WORK}"
fi
export XDG_CONFIG_HOME="${HOME}/.config"

export DISTRO_TYPE='unknown'
if [ -r /etc/debian_version ]; then
  DISTRO_TYPE='debian'
elif [ -r /etc/arch-release ]; then
  DISTRO_TYPE='arch'
fi

# Set terminal property (used by msgid-chooser).
export COLORTERM="yes"
export CLICOLOR=1
export PAGER=${PAGER:-less}

# Editors
export ALTERNATE_EDITOR="emacs"
export EDITOR="emacsclient -nw"
export VISUAL="emacsclient --alternate-editor=emacs"
export DOOMDIR="${DOTFILES_HOME}/doom"

# Go setup
export GOPATH='/go'
if [ "$OS_TYPE" = 'Darwin' ]; then
  GOPATH='/opt/go'
fi


# Linux brew setup
if [ -d '/home/linuxbrew/.linuxbrew' ]; then
  export HOMEBREW_PREFIX="/home/linuxbrew/.linuxbrew";
  export HOMEBREW_CELLAR="/home/linuxbrew/.linuxbrew/Cellar";
  export HOMEBREW_REPOSITORY="/home/linuxbrew/.linuxbrew/Homebrew";
  export PATH="/home/linuxbrew/.linuxbrew/bin:/home/linuxbrew/.linuxbrew/sbin${PATH+:$PATH}";
  export MANPATH="/home/linuxbrew/.linuxbrew/share/man${MANPATH+:$MANPATH}:";
  export INFOPATH="/home/linuxbrew/.linuxbrew/share/info:${INFOPATH:-}";
fi

# Ruby setup
GEM_HOME="$HOME/.gems"
if [ -d "$GEM_HOME" ]; then
  export GEM_HOME
fi

ANDROID_SDK_ROOT=/d/android/sdk
if [ -d "$ANDROID_SDK_ROOT" ]; then
  export ANDROID_SDK_ROOT
fi

# For zsh compatibility with bash.
if [ -z "$HOSTNAME" ]; then
  export HOSTNAME=$HOST
fi

if [ "${OS_TYPE}" != 'Darwin' ]; then
  if [ ! -d "${HOME}/.terminfo" ]; then
    print "Adding xterm-24bit as terminal description."
    /usr/bin/tic -x -o "${HOME}/.terminfo" "${DOTFILES_HOME}/terminfo/xterm-24bit.terminfo"
  fi
  export TERM=xterm-24bit
fi

# Set the NPM auth token if it exists.
npm_auth_token_file="$HOME/.config/npm/npm-auth-token"
if [ -f "$npm_auth_token_file" ]; then
  NPM_AUTH_TOKEN="$(cat "${HOME}/.config/npm/npm-auth-token")"
  export NPM_AUTH_TOKEN
fi
unset npm_auth_token_file

# Mac specific
if [ "${OS_TYPE}" = 'Darwin' ]; then
  github_personal_token_file="$HOME/.config/github/personal-token"
  if [ -f "$github_personal_token_file" ]; then
    HOMEBREW_GITHUB_API_TOKEN="$(cat "${github_personal_token_file}")"
    export HOMEBREW_GITHUB_API_TOKEN
  fi
  unset github_personal_token_file
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
  /opt/homebrew/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/bin \
  "${HOME}/.pub-cache/bin" \
  /snap/bin/ \
  "/usr/share/texmf-dist/scripts/texlive" \
  "/usr/local/bin" \
  "${GEM_HOME}/bin" \
  "/opt/homebrew/bin" \
  "${HOME}/.yarn/bin" \
  "${HOME}/.cargo/bin" \
  '/d/node/bin' \
  "${HOME}/.cask/bin" \
  "${GOPATH}/bin" \
  "${HOME}/.cargo/bin" \
  "/d/go/bin" \
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
  #
  # read -rd is safe with dash.
  # shellcheck disable=SC2039
  {
    PATH="$(echo "$PATH" | {
      P=""
      while read -rd: dir; do
        if [ "$dir" = '/usr/games' ] || [ "$dir" = '/usr/local/games' ]; then
          continue
        elif [ -d "$dir" ]; then
          P="$P:$dir"
        fi
      done
      echo "${P#?}"
    })"
  } 3>&1
  IFS="$oldIFS"
}
clean_path && unset -f clean_path
