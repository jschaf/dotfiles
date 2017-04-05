#!/bin/zsh

# Disable autocompletion setup by Google /etc/zshrc
# google_zsh_flysolo='I march to my own drum'

# Uncomment to profile ZSH startup, or use the `profile` function.
# ZSH_PROFILE_RC=1


# Sources the supplied file and captures timing information when profiling.
function xsource() {
  if is-profiling-zshrc; then
    float start_time=${EPOCHREALTIME}
    builtin source "$1"
    float end_time=${EPOCHREALTIME}
    float elapsed_time=$(((end_time - start_time) * 1000))
    printf "% 3.0fms - $1\n" ${elapsed_time}
  else
    builtin source "$1"
  fi
}

# Initialize setup for profiling ZSH startup.
if [[ $ZSH_PROFILE_RC -gt 0 ]]; then
    print "Profiling results in order of execution:"
    # Need datetime for EPOCHREALTIME to get good precision without using date.
    zmodload zsh/datetime
    zmodload zsh/zprof
    float zshenv_start_time=${EPOCHREALTIME}
    float -gx _RC_START_TIME=${EPOCHREALTIME}
    # Override definitions to profile /etc/* files
    function .() {
      xsource "$@"
    }

    function source() {
      xsource "$@"
    }
fi

# Setup function and completion directories
zshrc_fpath=("${ZDOTDIR}/completions" "${ZDOTDIR}/functions" "${ZDOTDIR}/work")

# Autoload all shell functions from all directories in $zshrc_fpath (following
# symlinks) that have the executable bit set.  The executable bit is not
# necessary, but gives you an easy way to stop the autoloading of a particular
# shell function.
for func in $^zshrc_fpath/*(N-.x:t); do
  autoload $func;
done

fpath=($zshrc_fpath $fpath)
unset zshrc_fpath

# General Settings
export LANG=en_US.UTF-8
export TERM="xterm-256color"
export ALTERNATE_EDITOR="emacs"
export EDITOR="emacsclient -a emacs"
export VISUAL="emacsclient -a emacs"
export FPP_EDITOR="emacsclient --no-wait -a emacs"
export WORKON_HOME="$HOME/.dotfiles/home/.virtualenvs"
export PROJECT_HOME="$HOME/prog"
export DOTFILES_HOME="${HOME}/.dotfiles"
export TMUXP_CONFIGDIR="${HOME}/.dotfiles/tmuxp"
export ZDOTDIR="${HOME}/.zsh"

# Go setup
export GOPATH="${HOME}/prog"

# NodeJS and NPM setup.
export NPM_PACKAGES="${HOME}/.npm-packages"
export NODE_PATH="${NPM_PACKAGES}/lib/node_modules:${NODE_PATH}"

# Ruby setup
export GEM_HOME="$HOME/.gems"

export RUST_SRC_PATH="${HOME}/.multirust/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src"
# Path Setup
#
# ZSH ties the $path array variable to the $PATH environmental variable via
#`typeset -T`.  We can make the $path array only have unique entries with
#`typeset -U`

# automatically remove duplicates from these arrays
typeset -U path PATH cdpath CDPATH fpath FPATH manpath MANPATH
path=(
    ~/bin-system
    ~/bin
    ~/prog/bin
    ~/homebrew/bin
    ~/.cask/bin
    ~/.cargo/bin
    ~/.yarn/bin
    # Setup Ruby and Gem so we install packages without root.
    ${GEM_HOME}/bin
    # Setup NPM so we can install global packages without root.  See
    # http://stackoverflow.com/questions/10081293.
    ${NPM_PACKAGES}/bin
    /usr/local/bin
    /usr/share/texmf-dist/scripts/texlive
    $path
)

manpath=(
  ${NPM_PACKAGES}/share/man
  /usr/man
  /usr/local/man
  $manpath
)

if is-profiling-zshrc ; then
    float zshenv_end_time=${EPOCHREALTIME}
    float zshenv_elapsed_time=$(((zshenv_end_time - zshenv_start_time) * 1000))
    printf "% 3.0fms - %s\n" ${zshenv_elapsed_time} \
           "${HOME}/.dotfiles/zsh/.zshenv"

fi
