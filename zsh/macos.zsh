#!/bin/zsh

if ! is-macos; then
  return
fi

# Overwrite ZSH log builtin to access macOS log viewer.
alias log=/usr/bin/log
