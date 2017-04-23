#!/bin/zsh

# Disable autocompletion setup by Google /etc/zshrc
# google_zsh_flysolo='I march to my own drum'

export ZDOTDIR="${HOME}/.zsh"

# Uncomment to profile ZSH startup, or use the `profile-zsh` function.
# ZSH_PROFILE_RC=1

function is-profiling-zsh() {
  [[ $ZSH_PROFILE_RC -gt 0 ]]
}

# Initialize setup for profiling ZSH startup.
if is-profiling-zsh; then
    source "${ZDOTDIR}/zsup.zsh"
    zsup-beginning-of-startup-file
fi

# Setup function and completion directories
zshrc_fpath=("${ZDOTDIR}/completions" "${ZDOTDIR}/functions" "${ZDOTDIR}/work")

# Autoload all shell functions from all directories in $zshrc_fpath (following
# symlinks) that have the executable bit set.  The executable bit is not
# necessary, but gives you an easy way to stop the autoloading of a particular
# shell function.
for func in $^zshrc_fpath/*(N-.x:t); do
  autoload -Uz $func;
done

fpath=($zshrc_fpath $fpath)
unset zshrc_fpath func

is-profiling-zsh && zsup-end-of-startup-file
