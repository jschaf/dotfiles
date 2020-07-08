#!/bin/zsh

if [[ "${_SOURCED_PROFILE}" != 'yes' ]]; then
  emulate sh -c "${HOME}/.profile"
fi

export _SOURCED_ZSH_ZPROFILE='yes'

# Force PATH to be unique; the path array is tied to $PATH (typeset -t).
# shellcheck disable=SC2034
typeset -U path

# NOTE: on MacOS, we'll read /etc/zprofile after this which runs path_helper and
# prepends the contents of /etc/paths and /etc/paths.d/* to $PATH effectively
# overriding our config.

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
  # /usr/share/zsh/functions/Completion/Unix
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
  # /usr/share/zsh/functions/VCS_Info
  # /usr/share/zsh/functions/VCS_Info/Backends
  /usr/share/zsh/functions/Zftp
  # /usr/share/zsh/functions/Zle
)
# Remove entries in fpath that are part of the array fpath_remove.
# https://stackoverflow.com/a/52188874
# shellcheck disable=SC2206
fpath=(
  ${fpath:|fpath_remove}
)
unset fpath_remove

# Load common functions by absolute path to avoid searching every entry in $fpath.
autoload /usr/share/zsh/functions/Completion/compinit
autoload /usr/share/zsh/functions/Prompts/promptinit
autoload /usr/share/zsh/functions/Misc/add-zsh-hook
autoload /usr/share/zsh/functions/Misc/add-zle-hook-widget
autoload "${DOTFILES_HOME}/zsh/functions/external-command-exists"
autoload "${DOTFILES_HOME}/zsh/functions/command-exists"
autoload "${DOTFILES_HOME}/zsh/prompts/prompt_pure_setup"
autoload "${DOTFILES_HOME}/zsh/functions/salias"

# http://zsh.sourceforge.net/Doc/Release/Parameters.html
# $'STR' expands escape sequences: http://zsh.sourceforge.net/Guide/zshguide05.html#l115
export TIMEFMT=$'\nreal\t%*E\nuser\t%*U\nsys\t%*S\nmaxmem\t%M MB\nfaults\t%F'
