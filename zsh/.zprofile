#!/bin/zsh


# Locale
export LANG=c
export LC_ALL=C
export LC_MESSAGES=C

# Editors
export PAGER=less
export ALTERNATE_EDITOR="emacs"
export EDITOR="emacsclient -nw"
export VISUAL="emacsclient --alternate-editor=emacs"
export DOOMDIR="${DOTFILES_HOME}/doom"

# Force PATH to be unique; the path array is tied to $PATH (typeset -t).
# shellcheck disable=SC2034
typeset -U path

if is-macos; then
  path+=(
    /usr/local/bin
    /System/Cryptexes/App/usr/bin
    /usr/bin
    /bin
    /usr/sbin
    /sbin
    /var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/local/bin
    /var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/bin
    /var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/appleinternal/bin
    /Library/Apple/usr/bin
    /opt/homebrew/bin
    "$HOME/bin"
    "$HOME/.orbstack/bin"
    "$HOME/.local/bin"
    "$HOME/go/bin"
    "$HOME/Library/Application Support/JetBrains/Toolbox/scripts"
    "/opt/homebrew/opt/postgresql@16/bin"
  )
  fpath[1,0]="/opt/homebrew/share/zsh/site-functions"
  fpath+=("/Applications/OrbStack.app/Contents/Resources/completions/zsh")
  export MANPATH="/opt/homebrew/share/man:$MANPATH"
fi

# Activate mise wherever it is installed, not only on macOS. This must follow
# the PATH setup above so Homebrew's mise is discoverable.
# Cache activation code, not hook-env output: project tools and environment must
# still refresh on startup, directory changes, and before every prompt.
function activate-mise() {
  local mise_bin=${commands[mise]:A}
  local cache_dir="$ZDOTDIR/.cache"
  local cache_file="$cache_dir/mise-activate.zsh"
  local key="$mise_bin|$PATH|$HOME|$ZSH_VERSION" cached_key script dir file
  local -A info
  local -a configs

  # Custom mise/XDG environments can change code generation. Use mise directly
  # in those sessions instead of reusing code generated under default settings.
  if [[ -n ${(M)${(k)parameters}:#(MISE_*|__MISE_*|XDG_*)} ]] ||
     ! zmodload -F zsh/stat b:zstat; then
    eval "$(mise activate zsh)"
    return
  fi

  # Follow binary replacement (including Homebrew symlink upgrades) and config
  # edits, creation, and deletion. Include ancestors for per-directory settings.
  configs=("$mise_bin" "$HOME/.config/mise/config.toml"
    "$HOME/.config/mise/settings.toml" "$HOME/.config/mise/conf.d" "$HOME/.config/mise/conf.d"/*.toml(N)
    /etc/mise/config.toml /etc/mise/conf.d /etc/mise/conf.d/*.toml(N)
    "$HOME/.config/mise/config.local.toml" "$HOME/.mise/config.toml"
    "$HOME/.tool-versions")
  dir=$PWD
  while true; do
    configs+=("$dir"/mise{,.local}.toml "$dir"/.mise{,.local}.toml
      "$dir"/.config/mise/config{,.local}.toml "$dir"/.mise/config{,.local}.toml
      "$dir/.miserc.toml" "$dir/.tool-versions")
    [[ $dir == / ]] && break
    dir=${dir:h}
  done
  for file in $configs; do
    if zstat -H info -- "$file" 2>/dev/null; then
      key+="|$file:$info[device]:$info[inode]:$info[size]:$info[mtime]:$info[ctime]"
    else
      key+="|$file:missing"
    fi
  done

  key=${(qqqq)key}
  if [[ -r $cache_file ]]; then
    # Read once: another shell may replace the file with a different PATH key.
    script=$(<"$cache_file")
    cached_key=${script%%$'\n'*}
    if [[ $cached_key == "# $key" ]]; then
      eval "$script"
      return
    fi
  fi

  # Evaluate the freshly generated output even if the cache is unwritable.
  # Rename a private temporary file so concurrent shells never read half a file.
  script=$(mise activate zsh) || return
  if (umask 077; command mkdir -p -- "$cache_dir" &&
      { print -r -- "# $key"; print -r -- "$script"; } > "$cache_file.$$" &&
      command mv -f -- "$cache_file.$$" "$cache_file") 2>/dev/null; then
    :
  else
    command rm -f -- "$cache_file.$$" 2>/dev/null
  fi
  eval "$script"
}
if command -v mise > /dev/null; then
  activate-mise
fi
unfunction activate-mise


# Print more info for time command.
# https://unix.stackexchange.com/a/562651/179300
#
# wall-time:   2700ms
# user-time:   243ms
# kernel-time: 2260ms
# cpu-percent: 92%  (243ms + 2260ms)/2700ms
# max-memory:  36320kB
# page-faults: 291 major, 84820 minor
# input-ops:   0
# output-ops:  0
# ctx-switch:  3675 voluntary, 22249 involuntary
# shellcheck disable=SC2034
TIMEFMT=$'\n wall-time:   %mE \n user-time:   %mU\n kernel-time: %mS \n cpu-percent: %P  (%mU + %mS)/%mE \n max-memory:  %MkB \n page-faults: %F major, %R minor \n input-ops:   %I \n output-ops:  %O \n ctx-switch:  %w voluntary, %c involuntary\n'

# Hide homebrew hints
export HOMEBREW_NO_ENV_HINTS=1
