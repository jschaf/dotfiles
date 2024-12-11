#!/bin/zsh

# Enable a fancy prompt.
function setup-prompt() {
  if [[ "$TERM" == "dumb" ]]; then
    PS1='$ '
  fi

  local prompt_symbol='λ'
  if is-tty; then
    # We probably can't support UTF-8.
    prompt_symbol='$'
  fi

  PS1='
%F{blue}%~%f
'                           # path on new line
  PS1+='%(?..%F{red}%?%f )' # exit status if non-zero
  PS1+="%F{cyan}${prompt_symbol}%f "

  # shellcheck disable=SC2034
  RPROMPT='%F{240}%D{%L:%M:%S}%f'

  # Secondary prompt, printed when the shell needs more information to
  # complete a command.
  PS2='\`%_> '
  # Selection prompt used within a select loop.
  PS3='?# '
  # The execution trace prompt (setopt xtrace). default: '+%N:%i>'
  PS4='+%N:%i:%_> '
}

# Package Setup

# fzf is a general-purpose command-line fuzzy finder.
function setup-fzf() {
  export FZF_COMMAND=fzf

  typeset -ax FZF_FIND_PRUNE_ARRAY
  FZF_FIND_PRUNE_ARRAY=(
    \(
    -path '*/\.*'
    -or -fstype 'devfs'
    -or -fstype 'devtmpfs'
    -or -fstype 'proc'
    -or -name 'bazel-*'
    -or -name 'node_modules'
    -or -name 'READONLY'
    -or -name 'vendor'
    \) -prune
  )
  local base03="234"
  local base02="235"
  local base01="240"
  local base00="241"
  local base0="244"
  local base1="245"
  local base2="254"
  local base3="230"
  local yellow="136"
  local orange="166"
  local red="160"
  local magenta="125"
  local violet="61"
  local blue="33"
  local cyan="37"
  local green="64"

  # Solarized Dark color scheme for fzf
  local -a solarized_dark_colors
  solarized_dark_color_map=(
    "fg:-1"
    "bg:-1"
    "hl:$blue"
    "fg+:$base2"
    "bg+:$base02"
    "hl+:$blue"
    "info:$blue"
    "prompt:$blue"
    "pointer:$base3"
    "marker:$base3"
    "spinner:$yellow"
  )
  local solarized_dark_fzf="--color ${(j:,:)solarized_dark_color_map}"

  local -a fzf_custom_options
  fzf_custom_options=(
    --height 40%
    --bind ctrl-space:toggle+down
    --bind ctrl-o:jump
    # Need to preserve quotes when expanded.
    # Ctrl-c, ctrl-g and esc default to abort, which clears the
    # current prompt and returns 1.  Cancel simply gives focus back to
    # the shell, leaving the text.
    '--bind "ctrl-y:execute-silent(print -- {+} | clipboard-copy)+abort,ctrl-c:cancel,ctrl-g:cancel,esc:cancel"'
  )
  # FZF reads options from FZF_DEFAULT_OPTS.
  export FZF_DEFAULT_OPTS="$solarized_dark_fzf ${(j: :)fzf_custom_options}"
}

# Disable highlight of pasted text.
zle_highlight+=(paste:none)

function setup-tmux-integration() {
  # https://superuser.com/questions/739391/disallowing-windows-to-rename-themselves-in-tmux
  export DISABLE_AUTO_TITLE='true'
}

function setup-zoxide() {
  if ! command -v zoxide &>/dev/null; then
    return
  fi

  function z() {
    if [[ "$#" -eq 0 ]]; then
      builtin cd ~ || return 1
    elif [[ "$#" -eq 1 ]] && { [[ -d "$1" ]] || [[ "$1" = '-' ]] || [[ "$1" =~ ^[-+][0-9]$ ]]; }; then
      builtin cd "$1" || return 1
    elif [[ "$#" -eq 2 ]] && [[ "$1" = "--" ]]; then
      builtin cd "$2" || return 1
    else
      local result
      result="$(zoxide query --exclude "$(pwd -L)" -- "$@")" && cd "${result}" || return 1
    fi
  }

  function zi() {
    local result
    result="$(zoxide query --interactive -- "$@")" && builtin cd "${result}" || return 1
  }

  function __zoxide_hook() {
    zoxide add "$(pwd -P)"
  }

  chpwd_functions+=(__zoxide_hook)

}

setup-prompt && unfunction setup-prompt

setup-fzf && unfunction setup-fzf

setup-tmux-integration && unfunction setup-tmux-integration

setup-zoxide && unfunction setup-zoxide
