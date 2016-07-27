function update-dotfiles() {
    echo "$fg[white]Updating ~/.dotfiles $reset_color"
    cd ~/.dotfiles
    git stash
    git pull origin master
    git stash pop
    cd -
}

function update-dotfile-symlinks() {
    echo "$fg[white]Updating symlinks to ~/.dotfiles$reset_color"
    if [[ "$(expr substr $(uname -s) 1 5)" == 'Linux' ]]; then
        rcup -t linux
    else
        rcup
    fi
}

# Command to get this workstation synchronized.
function open-sesame() {
    echo "$fg[white]Welcome back! Lets get you up to speed...$reset_color"
    if which-command "open-sesame-system" > /dev/null; then
        open-sesame-system
        echo
    else
        echo "$fg[white]No system specific function found.$reset_color"
    fi
    update-dotfiles
    echo
    update-dotfile-symlinks
    echo
    echo "$fg[green]You're five-by-five, good-to-go.$reset_color"
}
