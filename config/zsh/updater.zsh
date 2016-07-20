function update-dotfiles() {
    echo "$fg[white]Updating ~/.dotfiles $reset_color"
    cd ~/.dotfiles
    git stash
    git pull origin master
    git stash pop
    cd -
}

# Command to get this workstation synchronized.
function open-sesame() {
    echo "$fg[white]Welcome back! Lets get you up to speed...$reset_color"
    update-dotfiles
    echo
    echo "$fg[green]You're five-by-five, good-to-go.$reset_color"
}
