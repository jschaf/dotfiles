#!/bin/zsh

export HISTFILE="${ZDOTDIR}/.zsh_history"

# Read the history from HISTFILE.
fc -R

# -l: List history on STDOUT.
# -n: Suppress history numbers
# -r: Reverse history so the latest entry is the first line.
fc -lnr -10
