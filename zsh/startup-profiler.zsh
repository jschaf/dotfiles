#!/bin/zsh

# Profiling functions for ZSH startup.

# An array of files that were profiled.
# The files are ordered by time they were sourced.
typeset -ax ZSUP_FILES

# An associative array from file names to the timestamp of when the file was
# started to be sourced.
typeset -Ax ZSUP_START_TIMESTAMPS

# An associative array from file names to the timestamp of when the file was
# done being sourced.
typeset -Ax ZSUP_END_TIMESTAMPS

# An associative array from file names to the time taken to source a file.
typeset -Ax ZSUP_ELAPSED_TIMESTAMPS

# An associative array from file names to the depth of the file.
# In Python style: {"~/.zshenv": 0, "~/my-file.zsh": 2}
typeset -Ax ZSUP_DEPTHS

# The current depth of nested source calls.
# Used to display nested files when we report the profiling times.
integer -x ZSUP_DEPTH=-1

# Set this variable to enable debug statements.
ZSUP_DEBUG=1

# Necessary for EPOCHREALTIME to get good precision without shelling out to
# `date`.
zmodload zsh/datetime

# Necessary to get by function profiling information.
zmodload zsh/zprof

# The earliest start time.  We use this as time 0.
float ZSUP_INIT_TIMESTAMP=${EPOCHREALTIME}

function zsup-debug() {
  if [[ -z $ZSUP_DEBUG ]]; then
    return
  fi
  # We don't want zsup-debug, we want the caller.  This doesn't work with
  # emulate -L sh.
  local current_function="$funcstack[2]"
  print "ZSUP: $current_function $1" 1>&2
}

function zsup-error() {
  print "ERROR: $1" 1>&2
}

function zsup-is-login-shell() {
  [[ -o login ]]
}

function zsup-is-interactive-shell() {
  [[ -o interactive ]]
}

function zsup-start-profiling-file() {
  local file="$1"
  ZSUP_FILES+=("${file}")
  ZSUP_DEPTH+=1
  ZSUP_DEPTHS[${file}]=$ZSUP_DEPTH
  # Convert to milliseconds early to save precision.
  float start_time=$(( (EPOCHREALTIME - ZSUP_INIT_TIMESTAMP) * 1000 ))
  ZSUP_START_TIMESTAMPS[${file}]=$start_time
  zsup-debug "depth=$ZSUP_DEPTH, start_time=$start_time, file=$file"
}

function zsup-end-profiling-file() {
  local file="$1"
  ZSUP_DEPTH=$((ZSUP_DEPTH - 1))
  float end_time=$(( (EPOCHREALTIME - ZSUP_INIT_TIMESTAMP) * 1000 ))
  ZSUP_END_TIMESTAMPS[${file}]="$end_time"
  float start_time=$ZSUP_START_TIMESTAMPS[$file]
  ZSUP_ELAPSED_TIMESTAMPS[$file]=$((end_time - start_time))
  zsup-debug "depth=$ZSUP_DEPTH, end_time=$end_time, file=$file"
}

# Sources the supplied file and captures timing information.
function zsup-source() {
  local file_to_source="$1"
  zsup-debug "file=$file_to_source"
  zsup-start-profiling-file "$file_to_source"
  builtin source "$file_to_source"
  zsup-end-profiling-file "$file_to_source"
}

# Reset depth.  All startup files should start at depth 0.
# If depth is not 0, warn because something went wrong.
function zsup-reset-depth() {
  if [[ $ZSUP_DEPTH -gt 0 ]]; then
    zsup-error "ZSDUP_DEPTH is greater than 0 when profiling at $funcstack."
  fi
  ZSUP_DEPTH=0
}

# The expected next /etc/zsh/* startup file.
export ZSUP_NEXT_STARTUP_FILE

function zsup-files-similar() {
  local etc_startup="$1"
  local startup="$2"
  # Remove leading period.
  local startup_tail="${startup:t:s/./}"
  local etc_tail="${etc_startup:t}"
  zsup-debug "startup=$startup_tail, etc=$etc_tail"
  [[ "${etc_tail}" == "${startup_tail}" ]]
}

function zsup-beginning-of-startup-file() {
  local startup_file="$funcstack[-1]"

  zsup-debug "ZSUP_NEXT_STARTUP_FILE=$ZSUP_NEXT_STARTUP_FILE, startup_file=$startup_file"
  if zsup-files-similar "${ZSUP_NEXT_STARTUP_FILE}" "$startup_file"; then
    zsup-end-profiling-file "$ZSUP_NEXT_STARTUP_FILE"
  else
    zsup-debug "not similar: $ZSUP_NEXT_STARTUP_FILE $startup_file"
  fi
  ZSUP_NEXT_STARTUP_FILE=''


  zsup-start-profiling-file "$startup_file"
  zsup-reset-depth
}

function zsup-end-of-startup-file() {
  local startup_file="$funcstack[-1]"
  zsup-reset-depth
  zsup-end-profiling-file "$startup_file"

  # Start timing for /etc/ based zsh startup files.  We can infer what's going
  # to get run next with some cleverness.
  local next_startup_file=$(zsup-infer-next-startup-file "$startup_file")
  if [[ "$next_startup_file" == "DONE" ]]; then
     return
  fi
  ZSUP_NEXT_STARTUP_FILE="/etc/zsh/$next_startup_file"
  zsup-start-profiling-file "$ZSUP_NEXT_STARTUP_FILE"
}

# The state diagram transitions between startup files.
# Which state comes next depends on three things:
# 1. Is the shell interactive?
# 2. Is the shell a login shell?
# 3. Is NO_RCS set?
typeset -Ax ZSUP_STATES

function zsup-init-zsup-states() {
  # Use the tail of the file name because we don't know for sure what directory
  # the files are in, e.g. /etc/zsh/zshrc vs /etc/zshrc.  The /etc/ zsh files
  # don't have a leading dot
  local etc_zshenv='zshenv'
  local zshenv='.zshenv'
  local etc_zprofile='zprofile'
  local zprofile='.zprofile'
  local etc_zshrc='zshrc'
  local zshrc='.zshrc'
  local etc_zlogin='zlogin'
  local zlogin='.zlogin'

  # ZSUP_STATES["$etc_zshenv:NOLOGIN:NOINTERACTIVE"]="$zshenv"
  # ZSUP_STATES["$etc_zshenv:NOLOGIN:INTERACTIVE"]="$zshenv"
  # ZSUP_STATES["$etc_zshenv:LOGIN:NOINTERACTIVE"]="$zshenv"
  # ZSUP_STATES["$etc_zshenv:LOGIN:INTERACTIVE"]="$zshenv"

  ZSUP_STATES["$zshenv:NOLOGIN:NOINTERACTIVE"]="DONE"
  ZSUP_STATES["$zshenv:NOLOGIN:INTERACTIVE"]="$etc_zshrc"
  ZSUP_STATES["$zshenv:LOGIN:NOINTERACTIVE"]="$etc_zprofile"
  ZSUP_STATES["$zshenv:LOGIN:INTERACTIVE"]="$etc_zprofile"

  # ZSUP_STATES["$etc_zprofile:NOLOGIN:NOINTERACTIVE"]="ERROR"
  # ZSUP_STATES["$etc_zprofile:NOLOGIN:INTERACTIVE"]="ERROR"
  # ZSUP_STATES["$etc_zprofile:LOGIN:NOINTERACTIVE"]="$zprofile"
  # ZSUP_STATES["$etc_zprofile:LOGIN:INTERACTIVE"]="$zprofile"

  ZSUP_STATES["$zprofile:NOLOGIN:NOINTERACTIVE"]="ERROR"
  ZSUP_STATES["$zprofile:NOLOGIN:INTERACTIVE"]="ERROR"
  ZSUP_STATES["$zprofile:LOGIN:NOINTERACTIVE"]="$etc_zlogin"
  ZSUP_STATES["$zprofile:LOGIN:INTERACTIVE"]="$etc_zshrc"

  # ZSUP_STATES["$etc_zshrc:NOLOGIN:NOINTERACTIVE"]="ERROR"
  # ZSUP_STATES["$etc_zshrc:NOLOGIN:INTERACTIVE"]="$zshrc"
  # ZSUP_STATES["$etc_zshrc:LOGIN:NOINTERACTIVE"]="ERROR"
  # ZSUP_STATES["$etc_zshrc:LOGIN:INTERACTIVE"]="$zshrc"

  ZSUP_STATES["$zshrc:NOLOGIN:NOINTERACTIVE"]="ERROR"
  ZSUP_STATES["$zshrc:NOLOGIN:INTERACTIVE"]="DONE"
  ZSUP_STATES["$zshrc:LOGIN:NOINTERACTIVE"]="ERROR"
  ZSUP_STATES["$zshrc:LOGIN:INTERACTIVE"]="$etc_zlogin"

  # ZSUP_STATES["$etc_zlogin:NOLOGIN:NOINTERACTIVE"]="ERROR"
  # ZSUP_STATES["$etc_zlogin:NOLOGIN:INTERACTIVE"]="ERROR"
  # ZSUP_STATES["$etc_zlogin:LOGIN:NOINTERACTIVE"]="$zlogin"
  # ZSUP_STATES["$etc_zlogin:LOGIN:INTERACTIVE"]="$zlogin"

  ZSUP_STATES["$zlogin:NOLOGIN:NOINTERACTIVE"]="ERROR"
  ZSUP_STATES["$zlogin:NOLOGIN:INTERACTIVE"]="ERROR"
  ZSUP_STATES["$zlogin:LOGIN:NOINTERACTIVE"]="DONE"
  ZSUP_STATES["$zlogin:LOGIN:INTERACTIVE"]="DONE"
}
zsup-init-zsup-states

function zsup-current-startup-file-key() {
  local current_startup_file="${funcstack[-1]}"
  local file_tail="${current_startup_file:t}"
  local login="$(zsup-is-login-shell && print LOGIN || print NOLOGIN)"
  local interactive="$(zsup-is-interactive-shell && print INTERACTIVE || print NOINTERACTIVE)"
  local key="${file_tail}:${login}:${interactive}"
  zsup-debug "key=$key"
  print "$key"
}

function zsup-infer-next-startup-file() {
  local key="$(zsup-current-startup-file-key)"
  local next_startup_file=$ZSUP_STATES["$key"]
  zsup-debug "next_startup_file=$next_startup_file"
  if [[ "$next_startup_file" == 'ERROR' ]]; then
    zsup-error "Illegal state transition from $key."
    next_startup_file="DONE"
  fi

  print "$next_startup_file"
}

# Override definitions of builtins to profile manually sourced files.
function .() {
  zsup-source "$@"
}

function source() {
  zsup-source "$@"
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

typeset -Ax ZSUP_HEREMETIC_ELAPSED

function zsup-calc-hermetic-time() {
    local file="$1"
    integer depth=$ZSUP_DEPTHS[$file]
    float elapsed=$ZSUP_ELAPSED_TIMESTAMPS[$file]
    integer file_index=$ZSUP_FILES[(ei)$file]
    zsup-debug "file=$file, depth=$depth, elapsed=$elapsed, index=$file_index"
    integer child_index=$((file_index + 1))
    integer child_duration=0

    while [[ child_index -le $#ZSUP_FILES ]]; do
        local child_file=$ZSUP_FILES[$child_index]
        integer child_depth=$ZSUP_DEPTHS[$child_file]
        child_duration+=ZSUP_ELAPSED_TIMESTAMPS[$child_file]
        if [[ $child_depth -le $depth ]]; then
           break
        fi
        child_index+=1
    done
    ZSUP_HEREMETIC_ELAPSED[$file]=$((elapsed - child_duration))
}

function zsup-build-hermetic-times() {
    for file in $ZSUP_FILES; do
        print $file
        zsup-calc-hermetic-time $file
    done
}

function zsup-print-results() {
  zsup-build-hermetic-times
  for file in $ZSUP_FILES; do
    float elapsed_time=$ZSUP_HERMETIC_ELAPSED[$file]
    integer file_depth=$ZSUP_DEPTHS["$file"]
    local separator="$(zsup-string-repeat '  ' $file_depth)"
    local short_file="$(zsup-shorten-file-name $file)"
    printf "%4.1f ms  %s%s\n" $elapsed_time $separator $short_file
  done

  print
  print 'Use `zprof | less` for detailed results.'

  [[ -z $ZSUP_DEBUG ]] && zsup-cleanup-namespace
}

function zsup-cleanup-namespace() {

  unfunction zsup-source zsup-shorten-file-name zsup-string-repeat
  unset ZSUP_FILES ZSUP_END_TIMESTAMPS ZSUP_DEPTH
}
