#!/bin/zsh

# Profiling functions for ZSH startup.

# Necessary for EPOCHREALTIME to get good precision without shelling out to
# `date`.
zmodload zsh/datetime

# Necessary to get by function profiling information.
zmodload zsh/zprof

# An array of files that were profiled.
# The files are ordered by time they were sourced.
typeset -a ZSUP_FILES

# An associative array from file names to the timestamp of when the file was
# started to be sourced.
typeset -A ZSUP_START_TIMESTAMPS

# An associative array from file names to the timestamp of when the file was
# done being sourced.
typeset -A ZSUP_END_TIMESTAMPS

# An associative array from file names to the time taken to source a file.
# This includes the children.
typeset -A ZSUP_DURATIONS

# An associative array from file names to the time taken to source a file, *not*
# including its children.
typeset -A ZSUP_HERMETIC_DURATIONS

# An associative array from file names to the depth of the file.
# In Python style: {"~/.zshenv": 0, "~/my-file.zsh": 2}
typeset -A ZSUP_DEPTHS

# The state diagram transitions between startup files.
# Which state comes next depends on three things:
# 1. Is the shell interactive?
# 2. Is the shell a login shell?
# 3. Is NO_RCS set? This state is currently ignored.
#
# The format is:
#   key = $filename:LOGIN:INTERACTIVE
#   value = $next_filename
typeset -A ZSUP_STATES

# The current depth of nested source calls.
# Used to display nested files when we report the profiling times.
integer ZSUP_DEPTH=-1

# Set this variable to enable debug statements.
# ZSUP_DEBUG=1

# The earliest start time.  We use this as time 0.
float ZSUP_INIT_TIMESTAMP=${EPOCHREALTIME}

# Logs debug information to STDERR.
function zsup-debug() {
  [[ -z $ZSUP_DEBUG ]] && return
  # We don't want zsup-debug, we want the caller.  This doesn't work with
  # emulate -L sh.
  local current_function="$funcstack[2]"
  printf "ZSUP: %-30s  %s\n" $current_function $1 1>&2
}

# Logs an error.
function zsup-error() {
  print "ERROR: $1" 1>&2
}

# Starts collection information necessary to profile a file.
# Add the file to visited files, record the start time and depth.
function zsup-start-profiling-file() {
  local file="$1"
  ZSUP_FILES+=(${file})
  ZSUP_DEPTH+=1
  ZSUP_DEPTHS[$file]=$ZSUP_DEPTH
  # Convert to milliseconds early to save precision.
  float start_time=$(( (EPOCHREALTIME - ZSUP_INIT_TIMESTAMP) * 1000 ))
  ZSUP_START_TIMESTAMPS[$file]=$start_time
  zsup-debug "depth=$ZSUP_DEPTH, start_time=$start_time, file=$file"
}

# Completes collection of profiling information.
function zsup-end-profiling-file() {
  local file="$1"
  ZSUP_DEPTH=$((ZSUP_DEPTH - 1))
  float end_time=$(( (EPOCHREALTIME - ZSUP_INIT_TIMESTAMP) * 1000 ))
  ZSUP_END_TIMESTAMPS[$file]=$end_time
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

# Resets the depth counter.
# All startup files should start at depth 0. If depth is not 0, warn because
# something went wrong.
function zsup-reset-depth() {
  if [[ $ZSUP_DEPTH -gt 0 ]]; then
    zsup-error "ZSDUP_DEPTH is greater than 0 when profiling $funcstack[-1]."
  fi
  ZSUP_DEPTH=0
}

# The expected next /etc/zsh/* startup file.
export ZSUP_NEXT_STARTUP_FILE=''

# Profiles the user startup files.
# This is intended to be placed inside the user startup files, i.e. ~/.zshrc and
# friends.
function zsup-beginning-of-startup-file() {
  local startup_file="$funcstack[-1]"

  zsup-debug "STARTUP_FILE=$ZSUP_NEXT_STARTUP_FILE, startup_file=$startup_file"
  [[ -n $ZSUP_NEXT_STARTUP_FILE ]] && \
      zsup-end-profiling-file "$ZSUP_NEXT_STARTUP_FILE"
  ZSUP_NEXT_STARTUP_FILE=''

  zsup-start-profiling-file "$startup_file"
  zsup-reset-depth
}

# Profiles the user startup files.
# This is intended to be placed inside the user startup files, i.e. ~/.zshrc and
# friends.
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
  ZSUP_NEXT_STARTUP_FILE=$next_startup_file
  zsup-start-profiling-file "$next_startup_file"
}
# Figures out where zsh startup files are located.
typeset ZSUP_USER_DIR="${ZDOTDIR:-$HOME}"
typeset ZSUP_SYSTEM_DIR='/etc/zsh'
if [[ -e '/etc/zshrc' || -e '/etc/zshev' || -e '/etc/zprofile'
      || -e '/etc/zlogin' ]]; then
    ZSUP_SYSTEM_DIR='/etc'
fi

# Initialize the statue diagram.
function zsup-init-zsup-states() {
  local etc_zshenv="$ZSUP_SYSTEM_DIR/zshenv"
  local zshenv="$ZSUP_USER_DIR/.zshenv"
  local etc_zprofile="$ZSUP_SYSTEM_DIR/zprofile"
  local zprofile="$ZSUP_USER_DIR/.zprofile"
  local etc_zshrc="$ZSUP_SYSTEM_DIR/zshrc"
  local zshrc="$ZSUP_USER_DIR/.zshrc"
  local etc_zlogin="$ZSUP_SYSTEM_DIR/zlogin"
  local zlogin="$ZSUP_USER_DIR/.zlogin"

  ZSUP_STATES["$etc_zshenv:NOLOGIN:NOINTERACTIVE"]="$zshenv"
  ZSUP_STATES["$etc_zshenv:NOLOGIN:INTERACTIVE"]="$zshenv"
  ZSUP_STATES["$etc_zshenv:LOGIN:NOINTERACTIVE"]="$zshenv"
  ZSUP_STATES["$etc_zshenv:LOGIN:INTERACTIVE"]="$zshenv"

  ZSUP_STATES["$zshenv:NOLOGIN:NOINTERACTIVE"]="DONE"
  ZSUP_STATES["$zshenv:NOLOGIN:INTERACTIVE"]="$etc_zshrc"
  ZSUP_STATES["$zshenv:LOGIN:NOINTERACTIVE"]="$etc_zprofile"
  ZSUP_STATES["$zshenv:LOGIN:INTERACTIVE"]="$etc_zprofile"

  ZSUP_STATES["$etc_zprofile:NOLOGIN:NOINTERACTIVE"]="ERROR"
  ZSUP_STATES["$etc_zprofile:NOLOGIN:INTERACTIVE"]="ERROR"
  ZSUP_STATES["$etc_zprofile:LOGIN:NOINTERACTIVE"]="$zprofile"
  ZSUP_STATES["$etc_zprofile:LOGIN:INTERACTIVE"]="$zprofile"

  ZSUP_STATES["$zprofile:NOLOGIN:NOINTERACTIVE"]="ERROR"
  ZSUP_STATES["$zprofile:NOLOGIN:INTERACTIVE"]="ERROR"
  ZSUP_STATES["$zprofile:LOGIN:NOINTERACTIVE"]="$etc_zlogin"
  ZSUP_STATES["$zprofile:LOGIN:INTERACTIVE"]="$etc_zshrc"

  ZSUP_STATES["$etc_zshrc:NOLOGIN:NOINTERACTIVE"]="ERROR"
  ZSUP_STATES["$etc_zshrc:NOLOGIN:INTERACTIVE"]="$zshrc"
  ZSUP_STATES["$etc_zshrc:LOGIN:NOINTERACTIVE"]="ERROR"
  ZSUP_STATES["$etc_zshrc:LOGIN:INTERACTIVE"]="$zshrc"

  ZSUP_STATES["$zshrc:NOLOGIN:NOINTERACTIVE"]="ERROR"
  ZSUP_STATES["$zshrc:NOLOGIN:INTERACTIVE"]="DONE"
  ZSUP_STATES["$zshrc:LOGIN:NOINTERACTIVE"]="ERROR"
  ZSUP_STATES["$zshrc:LOGIN:INTERACTIVE"]="$etc_zlogin"

  ZSUP_STATES["$etc_zlogin:NOLOGIN:NOINTERACTIVE"]="ERROR"
  ZSUP_STATES["$etc_zlogin:NOLOGIN:INTERACTIVE"]="ERROR"
  ZSUP_STATES["$etc_zlogin:LOGIN:NOINTERACTIVE"]="$zlogin"
  ZSUP_STATES["$etc_zlogin:LOGIN:INTERACTIVE"]="$zlogin"

  ZSUP_STATES["$zlogin:NOLOGIN:NOINTERACTIVE"]="ERROR"
  ZSUP_STATES["$zlogin:NOLOGIN:INTERACTIVE"]="ERROR"
  ZSUP_STATES["$zlogin:LOGIN:NOINTERACTIVE"]="DONE"
  ZSUP_STATES["$zlogin:LOGIN:INTERACTIVE"]="DONE"
}
zsup-init-zsup-states

# Creates the current state as a key for ZSUP_STATES.  The key is printed.
function zsup-make-state-key() {
  local startup_file="$1"
  local login="NOLOGIN"
  if [[ -o login ]]; then login='LOGIN' fi
  local interactive="NOINTERACTIVE"
  if [[ -o interactive ]]; then interactive='INTERACTIVE' fi
  local key="${startup_file}:${login}:${interactive}"
  zsup-debug "key=$key"
  print "$key"
}

# From the current startup file, figure out what file will be sourced next.
# We need to infer what system files will get sourced next to display accurate
# profiling information.
function zsup-infer-next-startup-file() {
  local key="$(zsup-make-state-key $funcstack[-1])"
  local next_file=$ZSUP_STATES["$key"]

  while [[ ! -e $next_file || $next_file == "ERROR" ]]; do
      zsup-debug "skipping next_file=$next_file"
      if [[ $next_file == "ERROR" ]]; then
         zsup-error "Illegal file transition at '$key'."
         next_file="DONE"
      fi
      if [[ $next_file == "DONE" ]]; then
          break;
      fi
      key="$(zsup-make-state-key $next_file)"
      next_file=$ZSUP_STATES["$key"]
  done

  # If we have a user file next, profiling code should be in the file so don't
  # infer anything.
  if [[ "$next_file" != /etc/* ]]; then
    next_file="DONE"
  fi
  zsup-debug "next_file=$next_file"
  print "$next_file"
}

# Override definitions of builtins to profile manually sourced files.
function .() {
  zsup-source "$@"
}

function source() {
  zsup-source "$@"
}

# Calculates the time spent sourcing a file not including its children.
# The information is stored in ZSUP_HERMETIC_DURATIONS.
function zsup-calc-hermetic-time() {
    local file="$1"
    integer depth=$ZSUP_DEPTHS[$file]
    float elapsed=$ZSUP_DURATIONS[$file]
    integer file_index=$ZSUP_FILES[(ei)$file]
    zsup-debug "file=$file, depth=$depth, elapsed=$elapsed, index=$file_index"
    integer child_index=$((file_index + 1))
    float total_child_duration=0
    while [[ child_index -le $#ZSUP_FILES ]]; do
        local child_file=$ZSUP_FILES[$child_index]
        integer child_depth=$ZSUP_DEPTHS[$child_file]

        # We've reached a file at the same depth as $file, so stop.
        if [[ $child_depth -le $depth ]]; then
           break
        fi
        # We only add the direct children of the file because their duration
        # already includes all grandchildren.
        if (($child_depth == $depth + 1)); then
            float child_duration=$ZSUP_DURATIONS[$child_file]
           (( total_child_duration += child_duration ))
        fi
        child_index+=1
    done
    ZSUP_HERMETIC_DURATIONS[$file]=$((elapsed - total_child_duration))
}

# Populates ZSUP_DURATIONS and ZSUP_HERMETIC_DURATIONS.
function zsup-build-hermetic-times() {
    # Must be separate loops.
    for file in $ZSUP_FILES; do
        float start_time=$ZSUP_START_TIMESTAMPS[$file]
        float end_time=$ZSUP_END_TIMESTAMPS[$file]
        ZSUP_DURATIONS[$file]=$((end_time - start_time))
    done

    for file in $ZSUP_FILES; do
        zsup-calc-hermetic-time $file
    done
}

function zsup-print-results() {
  zsup-build-hermetic-times
  for file in $ZSUP_FILES; do
    float hermetic_time=$ZSUP_HERMETIC_DURATIONS[$file]
    integer file_depth=$ZSUP_DEPTHS[$file]

    local separator=''
    # Print 2 spaces $file_depth times.
    [[ $file_depth -gt 0 ]] && separator="$(printf '  %.0s' {1..$file_depth})"

    # Replace paths with $ZDOTDIR or $HOME.
    local short_file=$file
    [[ -n "$ZDOTDIR" ]] && short_file=${file//$ZDOTDIR/'$ZDOTDIR'}
    short_file=${short_file//$HOME/'~'}

    printf "%6.1f ms  %s%s\n" $hermetic_time $separator $short_file
  done

  # Join the array with +'s.
  float total_duration=$(( ${(j:+:)ZSUP_HERMETIC_DURATIONS} ))
  print
  printf "%6.1f ms  Total" $total_duration
  print
  print
  print 'Use `zprof | less` for detailed results.'
  [[ -z $ZSUP_DEBUG ]] && zsup-cleanup-namespace
}

function zsup-cleanup-namespace() {
    typeset -a zsup_functions=(
        zsup-beginning-of-startup-file
        zsup-build-elapsed-times
        zsup-build-hermetic-times
        zsup-calc-hermetic-time
        zsup-cleanup-namespace
        zsup-make-state-key
        zsup-debug
        zsup-end-of-startup-file
        zsup-end-profiling-file
        zsup-error
        zsup-files-similar
        zsup-infer-next-startup-file
        zsup-init-zsup-states
        zsup-print-results
        zsup-reset-depth
        zsup-source
        zsup-start-profiling-file
    )
    for func in $zsup_functions; do
        [[ -n ${functions[$func]} ]] && unfunction $func
    done
    typeset -a zsup_vars=(
        ZSUP_DEPTH
        ZSUP_DEPTHS
        ZSUP_DURATIONS
        ZSUP_END_TIMESTAMPS
        ZSUP_FILES
        ZSUP_HERMETIC_DURATIONS
        ZSUP_INIT_TIMESTAMP
        ZSUP_NEXT_STARTUP_FILE
        ZSUP_START_TIMESTAMPS
        ZSUP_STATES
        ZSUP_SYSTEM_DIR
        ZSUP_USER_DIR
    )
    for var in $zsup_vars; do
        unset "$var"
    done
}
