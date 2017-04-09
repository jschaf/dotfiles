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

# Shell
export ZDOTDIR="${HOME}/.zsh"


if is-profiling-zshrc ; then
    float zshenv_end_time=${EPOCHREALTIME}
    float zshenv_elapsed_time=$(((zshenv_end_time - zshenv_start_time) * 1000))
    printf "% 3.0fms - %s\n" ${zshenv_elapsed_time} \
           "${HOME}/.dotfiles/zsh/.zshenv"

fi
