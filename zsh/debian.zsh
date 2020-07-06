#!/bin/zsh

# Debian stuff

if ! is-debian; then
  return
fi

alias acs='apt-cache search'
alias acsh='apt-cache show'
alias acp='apt-cache policy'
salias adg="apt-get dist-upgrade"
salias agi="apt-get install"
salias agr="apt-get remove"
salias agu="apt-get upgrade"
salias ati="aptitude install"

salias agu="apt-get update"
alias dbp='dpkg-buildpackage'

# Sort installed Debian-packages by size.
if external-command-exists dpkg-query ; then
  # List installed Debian-packages sorted by size
  alias debs-by-size="dpkg-query -Wf 'x \${Installed-Size} \${Package} \${Status}\n' | sed -ne '/^x  /d' -e '/^x \(.*\) install ok installed$/s//\1/p' | sort -nr"
fi
