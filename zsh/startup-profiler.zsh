#!/bin/zsh

# Profiling functions for ZSH startup.

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
