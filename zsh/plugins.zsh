#!/bin/zsh

# Enable a fancy prompt.
function setup-prompt() {
  if [[ "$TERM" == "dumb" ]]; then
    setup-dumb-prompt-for-tramp
    return
  fi

  fpath+=($ZSH_DOTFILES/prompts)
  autoload -Uz promptinit && promptinit
  local fancy_prompt='λ'
  local plain_prompt='$'
  if is-tty; then
    # We probably can't support UTF-8.
    PURE_PROMPT_SYMBOL="$plain_prompt"
  else
    PURE_PROMPT_SYMBOL="$fancy_prompt"
  fi

  prompt pure

  # Secondary prompt, printed when the shell needs more information to
  # complete a command.
  PS2='\`%_> '
  # Selection prompt used within a select loop.
  PS3='?# '
  # The execution trace prompt (setopt xtrace). default: '+%N:%i>'
  PS4='+%N:%i:%_> '
}

# Disable all fancy prompt features when using a dumb prompt, like
# Emacs tramp.
function setup-dumb-prompt-for-tramp() {
  unsetopt zle
  unsetopt prompt_cr
  unsetopt prompt_subst
  if whence -w precmd >/dev/null; then
    unfunction precmd
  fi
  if whence -w preexec >/dev/null; then
    unfunction preexec
  fi
  PS1='$ '
}

# Package Setup

# fzf is a general-purpose command-line fuzzy finder.
function setup-fzf() {
  local fzfPath="${HOME}/.dotfiles/vendor/fzf"
  PATH+=":${fzfPath}/bin"
  MANPATH+=":${fzfPath}/man"

  export FZF_COMMAND=fzf

  typeset -ax FZF_FIND_PRUNE_ARRAY
  FZF_FIND_PRUNE_ARRAY=(
    \(
    -path '*/\.*'
    -or -fstype 'devfs'
    -or -fstype 'devtmpfs'
    -or -fstype 'proc'
    -or -name 'bazel-*'
    -or -name 'blaze-*'
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

# Setup PATH and completion for gcloud.
function setup-gcloud() {
  source-if-exists "${HOME}/google-cloud-sdk/path.zsh.inc"
  source-if-exists "${HOME}google-cloud-sdk/completion.zsh.inc"
}

function setup-zsh-async() {
  # Don't autoload because we need it immediately for the prompt.
  source-if-exists "${HOME}/.dotfiles/vendor/zsh-async/async.zsh"
}

function setup-fast-syntax-highlighting() {
  source "${HOME}/.dotfiles/vendor/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh"
}

function setup-tmux-package-manager() {
  local TPM_HOME="${HOME}/.tmux/plugins/tpm"
  if [[ ! -d "${TPM_HOME}" ]]; then
    print-error "tmux package manager (TPM) is not installed at ${TPM_HOME}"
  fi
}

# Tmux inside the st terminal doesn't like it when TERM isn't xterm-256 and
# won't properly display the background color.  fzf, on the other hand only
# likes TERM to screen-256color.  So, let TERM start as xterm-256color, but
# after tmux is started we can safely change TERM to screen-256color to appease
# fzf.
function setup-256-color-hack-for-fzf() {
  if [[ -n "$TMUX" ]]; then
    export TERM='screen-256color'
  fi
}

function setup-tmux-integration() {
  # https://superuser.com/questions/739391/disallowing-windows-to-rename-themselves-in-tmux
  export DISABLE_AUTO_TITLE='true'
}

setup-zsh-async && unfunction setup-zsh-async

setup-prompt && unfunction setup-prompt

setup-256-color-hack-for-fzf && unfunction setup-256-color-hack-for-fzf

setup-fzf && unfunction setup-fzf

setup-gcloud && unfunction setup-gcloud

setup-tmux-package-manager && unfunction setup-tmux-package-manager

setup-tmux-integration && unfunction setup-tmux-integration

setup-fast-syntax-highlighting && unfunction setup-fast-syntax-highlighting
