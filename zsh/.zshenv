#!/bin/zsh

# We do our own setup.
setopt no_global_rcs

# Variables
export DOTFILES_HOME="/opt/p/dotfiles"
export ZDOTDIR="${DOTFILES_HOME}/zsh"

# OS helpers are used by both login startup files and interactive non-login
# shells, such as the Codex embedded terminal.
export OS_TYPE
OS_TYPE="$(uname -s)"
function is-linux() { [[ "${OS_TYPE}" == "Linux" ]]; }
function is-darwin() { [[ "${OS_TYPE}" == "Darwin" ]]; }
function is-macos() { [[ "${OS_TYPE}" == "Darwin" ]]; }
function is-freebsd() { [[ "${OS_TYPE}" == "FreeBSD" ]]; }

# Returns 0 when running in a Linux framebuffer terminal.
function is-tty() {
  is-linux && [[ $(tty) == /dev/tty[0-9] ]]
}

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

# Claude Code sources $CLAUDE_ENV_FILE before every Bash tool command — after
# its shell snapshot restores a PATH captured from the launching app, which
# clobbers the prepend above. Point it at a static preamble that re-asserts
# the shims prepend inside agent sessions. SessionStart hooks still receive
# their own session-managed file (Claude Code overrides the variable in hook
# environments), so this is the user-global layer for repos without a hook.
# The desktop app captures its environment at launch: a change here reaches
# agent sessions only after the app is fully quit and relaunched.
export CLAUDE_ENV_FILE="${CLAUDE_ENV_FILE:-${ZDOTDIR}/claude-session-env.sh}"

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
