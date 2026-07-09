#!/bin/zsh

# We do our own setup.
setopt no_global_rcs

# Variables
export DOTFILES_HOME="/opt/p/dotfiles"
export ZDOTDIR="${DOTFILES_HOME}/zsh"

# Put mise shims ahead of everything (notably /opt/homebrew/bin) so tools
# pinned by a project's .mise.toml resolve correctly in non-interactive,
# non-login shells — e.g. Claude Code/Codex agent Bash calls, which read only
# .zshenv. Interactive login shells layer `mise activate zsh` on top in
# .zprofile; shims here + activate there is mise's documented hybrid setup.
# With no project pin the shim falls through to the system tool, so this only
# changes resolution where a pin exists.
if [[ -d "$HOME/.local/share/mise/shims" ]]; then
  export PATH="$HOME/.local/share/mise/shims:$PATH"
fi

function autoload-executables-in-dir() {
  local autoload_dir="$1"
  fpath=("${autoload_dir}" "${fpath[@]}")

  # Autoload all shell functions from in a given directory that have
  # the executable bit set. The executable bit is not necessary, but
  # gives you an easy way to stop the autoloading of a particular
  # shell function.
  for func in "${autoload_dir}"/*(N-.x:t); do
    autoload -Uz "$func"
  done
}

# Setup function and completion directories.
autoload-executables-in-dir "${ZDOTDIR}/completions"
autoload-executables-in-dir "${ZDOTDIR}/functions"
autoload-executables-in-dir "${ZDOTDIR}/iosource"
unfunction autoload-executables-in-dir
