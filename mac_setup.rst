===============
 Mac OSX Setup
===============

* Install Xcode via the app store
  
* Homebrew

    ruby -e "$(curl -fsSL https://raw.github.com/Homebrew/homebrew/go/install)"

* Install iterm2
  http://www.iterm2.com/

  # Set Option key as +Esc
  # Set Ctrl+Backspace to send hex code 17
  # Set Ctrl+Delete to send escape sequence d

* clone dependencies

    git clone git@github.com:jschaf/dotfiles.git .dotfiles
    sh ~/.dotfiles/setup-links.sh

    git clone git@github.com:jschaf/dotemacs.git .emacs.d
    
* install USB Overdrive

  # Set Button4 to Keyboard shortcut - Backward
  # Set Button5 to Keyboard shortcut - forward

  
Rebind key movements globally for mac. 
http://superuser.com/questions/366179/in-mac-can-i-remap-control-delete-or-control-w-to-act-as-option-delete

* Install basic tex, a slimmed down TeX Live

  # Allow tlmgr to update texlive without using sudo each time
  sudo chmod -R 777 /usr/local/texlive/2013basic/

