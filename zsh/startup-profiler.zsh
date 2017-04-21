#!/bin/zsh

# Profiling functions for ZSH startup.

# An array of files that were profiled.
#
# The files are ordered by time they were sourced.
typeset -ax ZSUP_FILES

# An associative array from file names to elapsed time to source the file in
# milliseconds.
#
# In Python style: {"~/.zshenv": 44, "~/my-file.zsh": 21.341}
typeset -Ax ZSUP_TIMINGS

# An associative array from a ZSH startup file to the time we began profiling
# it.
#
# Since we don't control how startup files like .zshenv and .zshrc are sourced,
# we need a way to record the startup time to reliably record the profile time
# for startup files.
typeset -Ax ZSUP_STARTUP_BEGIN_TIMES

# An associative array from file names to the time to the depth of the file.
#
# In Python style: {"~/.zshenv": 0, "~/my-file.zsh": 2}
typeset -Ax ZSUP_DEPTHS

# The current depth of nested source calls.
#
# Used to display nested files when we report the profiling times.
integer -x ZSUP_DEPTH=0

# Set this variable to enable debug statements.
# ZSUP_DEBUG=1

# Necessary for EPOCHREALTIME
zmodload zsh/datetime

# Necessary to get by function profiling information.
zmodload zsh/zprof

function zsup-debug() {
  if [[ -z $ZSUP_DEBUG ]]; then
    return
  fi
  # We don't want zsup-debug, we want the caller.
  local current_function=$funcstack[2]
  print "ZSUP_DEBUG: $current_function $1"
}

# Sources the supplied file and captures timing information.
function zsup-source() {
  # Act 1. The pledge.
  local file_to_source="$1"
  ZSUP_FILES+="${file_to_source}"
  ZSUP_DEPTH+=1
  ZSUP_DEPTHS["${file_to_source}"]=$ZSUP_DEPTH
  float start_time=${EPOCHREALTIME}
  zsup-debug "(begin): depth=$ZSUP_DEPTH, start_time=$start_time, \
$file_to_source"

  # Act 2. The turn.
  builtin source "${file_to_source}"

  # Act 3. The prestige.
  ZSUP_DEPTH=$((ZSUP_DEPTH - 1))
  float end_time=${EPOCHREALTIME}
  float elapsed_time=$(( (end_time - start_time) * 1000 ))
  ZSUP_TIMINGS["${file_to_source}"]="$elapsed_time"
  zsup-debug "(end): elapsed_time=$elapsed_time"
}

function zsup-beginning-of-startup-file() {
  # Get current file even if its .zshrc.  See
  # http://stackoverflow.com/questions/9901210/bash-source0-equivalent-in-zsh

  # TODO infer from funcstack if no arg
  local startup_file="$1"
  zsup-debug "(begin): $startup_file"

  # Reset depth.  If it's not 0, warn because something went wrong.
  if [[ $ZSUP_DEPTH -gt 0 ]]; then
    local message="ERROR: ZSDUP_DEPTH is greater than 0 when profiling "
    message+="$startup_file at zsup-beginning-of-startup-file."
    print "$message"
  fi
  ZSUP_DEPTH=0

  ZSUP_FILES+="${startup_file}"
  ZSUP_DEPTHS["${startup_file}"]=$ZSUP_DEPTH
  ZSUP_STARTUP_BEGIN_TIMES["${startup_file}"]=${EPOCHREALTIME}
  zsup-debug "(end): begin_time=${EPOCHREALTIME}"
}

function zsup-end-of-startup-file() {
  local startup_file="$1"
  zsup-debug "(begin): startup_file=$startup_file"

  # Reset depth.  If it's not 0, warn because something went wrong.
  if [[ $ZSUP_DEPTH -gt 0 ]]; then
    local message="ERROR: ZSDUP_DEPTH is greater than 0 when profiling "
    message+="$startup_file at zsup-end-of-startup-file."
    print "$message"
  fi
  ZSUP_DEPTH=0

  float end_time=${EPOCHREALTIME}
  local start_time=$ZSUP_STARTUP_BEGIN_TIMES["${startup_file}"]
  float elapsed_time=$(( (end_time - start_time) * 1000 ))
  ZSUP_TIMINGS["${startup_file}"]="$elapsed_time"
  zsup-debug "(end): end_time=$end_time, \
start_time=$start_time, elapsed_time=$elapsed_time"

  # Start timing for /etc/ based zsh startup files.  We can infer what's going
  # to get run next with some cleverness.
  zsup-infer-next-startup-file "$startup_file"
}

function zsup-infer-next-startup-file() {
  local current_startup_file="$1"
  local current_file="${current_startup_file:t}"
  zsup-debug "(begin): current_file=$current_file"

  if [[ "$current_file" == '.zshenv' ]]; then
    echo at .zshenv
    if zsup-is-login-shell; then
      echo 'infer /etc/zprofile'
    else
      echo 'infer /etc/zshrc'
    fi

  elif [[ "$current_file" == '.zprofile' ]]; then
    # We know we're a login shell.
    echo at .zprofile
    if zsup-is-interactive-shell; then
      echo 'infer /etc/zshrc'
    fi
    # We're skipping .zlogin, but that seems like a narrow use case.

  elif [[ "$current_file" == '.zshrc' ]]; then
    # Ignore .zlogin.
    return

  else
    print "ERROR: Unknown startup file: $current_startup_file"
  fi
}

# Override definitions of builtins to profile manually sourced files.
function .() {
  zsup-source "$@"
}

function source() {
  zsup-source "$@"
}

function zsup-is-login-shell() {
  [[ -o login ]]
}

function zsup-is-interactive-shell() {
  [[ -o interactive ]]
}

function zsup-dump-assoc-array() {
  local assoc_name="$1"
  for k in "${(Pk)$1}"; do
    echo -n "$k: ${(P)$1}[$k],"
    done
}

function zsup-shorten-file-name() {
  short_file="$1"
  [[ -n "$ZDOTDIR" ]] && short_file=${file//"$ZDOTDIR"/'$ZDOTDIR'}
  short_file=${short_file//$HOME/'~'}
  print $short_file
}

function zsup-string-repeat() {
  local char="$1"
  local repeat="$2"
  if [[ $repeat -lt 1 ]]; then
    return
  fi
  printf "$char%.0s" {1..$repeat}
}

function print-profile-results() {
  for file in $ZSUP_FILES; do
    local file_time=$ZSUP_TIMINGS["$file"]
    local file_depth=$ZSUP_DEPTHS["$file"]
    local separator="$(zsup-string-repeat '  ' $file_depth)"
    local short_file="$(zsup-shorten-file-name $file)"
    printf "%4.0fms  %s%s\n" $file_time $separator $short_file
  done

  print 'Use `zprof | less` for detailed results.'
  # Clean up namespace.
}

function zsup-cleanup-namespace() {
  [[ -n $ZSUP_DEBUG ]] && return

  unfunction zsupsource-profile zsup-shorten-file-name zsup-string-repeat
  unset ZSUP_FILES ZSUP_TIMINGS ZSUP_DEPTH
}
