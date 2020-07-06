#!/bin/zsh

# Arch Linux specific functionality
if ! is-arch-distro; then
  return
fi

alias kc="kubectl"

salias pms="pacman -S"
alias pas="pacaur -S"
alias pmss="pacman -Ss"
alias pass="pacaur -Ss"
salias pmr="pacman -R"
alias pmq="pacman -Q"
alias pmqi="pacman -Qi"


function arch-global-python2 {
  sudo rm /usr/bin/python
  sudo ln -s /usr/bin/python2.7 /usr/bin/python
  rehash
}

function arch-global-python3 {
  sudo rm /usr/bin/python
  sudo ln -s /usr/bin/python3.6 /usr/bin/python
  rehash
}
