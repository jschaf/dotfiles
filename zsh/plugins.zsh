#!/bin/zsh

function setup-prompt() {
  fpath+=($ZDOTDIR/prompts)
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

# Package Setup

# fzf is a general-purpose command-line fuzzy finder.
_gen_fzf_default_opts() {
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

  # Comment and uncomment below for the light theme.

  # Solarized Dark color scheme for fzf
  export FZF_DEFAULT_OPTS="
    --color fg:-1,bg:-1,hl:$blue,fg+:$base2,bg+:$base02,hl+:$blue
    --color info:$yellow,prompt:$yellow,pointer:$base3,marker:$base3,spinner:$yellow
  "
  ## Solarized Light color scheme for fzf
  #export FZF_DEFAULT_OPTS="
  #  --color fg:-1,bg:-1,hl:$blue,fg+:$base02,bg+:$base2,hl+:$blue
  #  --color info:$yellow,prompt:$yellow,pointer:$base03,marker:$base03,spinner:$yellow
  #"
}

function setup-fzf() {
  local fzfPath="${HOME}/.dotfiles/vendor/fzf"
  path+="${fzfPath}/bin"
  manpath+="${fzfPath}/man"

  _gen_fzf_default_opts
  # Key bindings
  source-if-exists "${fzfPath}/shell/key-bindings.zsh"

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
    \) -prune
  )
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