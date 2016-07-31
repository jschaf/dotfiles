#/bin/zsh

DOTFILES_DIR=${DOTFILES_DIR:-"${HOME}/.dotfiles"}
DOTFILES_VENDOR_DIR="${DOTFILES_DIR}/vendor"

function updater-print-info() {
    local message="$1"
    echo "$fg[white]${message}$reset_color"
}

function updater-print-success() {
    local message="$1"
    echo "$fg[green]${message}$reset_color"
}

# Return 0 if not uncommited changes, return 1 otherwise.
function git-repo-is-clean() {
    git diff-index --quiet HEAD --exit-code
}

function updater-pushd() {
    pushd "$@" > /dev/null
}


function updater-popd() {
    popd "$@" > /dev/null
}

function update-dotfiles() {
    updater-print-info "Updating ~/.dotfiles "
    updater-pushd "${DOTFILES_DIR}"
    git-repo-is-clean
    local needsStash=$?
    if [[ "${needsStash}" -eq 1 ]]; then
        updater-print-info "Stashing changes."
        git stash
    else
        updater-print-info "No changes to stash."
    fi
    git pull origin master
    if [[ "${needsStash}" -eq 1 ]]; then
        updater-print-info "Popping stash."
        git stash pop
    fi
    updater-popd
}

# Update the vendor/st repo.  It's special because it's origin is my github repo
# which has personal tweaks on the 'tweaks' branch.
function update-dotfile-vendor-st() {
    updater-pushd "${DOTFILES_VENDOR_DIR}/st"
    git checkout master
    git pull upstream master
    git push origin master
    git checkout tweaks
    git rebase origin/master
    git push -f origin tweaks
    updater-popd
}

# Update submodules in ~/.dotfiles/vendor and commit the changes.
function update-dotfile-vendors() {
    updater-pushd "${DOTFILES_DIR}"
    git submodule update --init
    git submodule foreach git pull origin master
    update-dotfile-vendor-st
    git add vendor
    git commit -m "chore(git): update submodules"
    updater-popd
}

function upgrade-dotfile-vendors() {
    updater-pushd "${DOTFILES_DIR}"
    # zgen is sourced by zshrc.

    # PathPicker - we only need to make sure a symlink exists.  -L means file exists
    # and is a symlink.
    if [[ ! -L "${HOME}/bin/fpp" ]]; then
        updater-print-info "Upgrading PathPicker."
        ln -s "${DOTFILES_VENDOR_DIR}/PathPicker/fpp" "${HOME}/bin"
    else
        updater-print-info "PathPicker is up to date."
    fi

    # fzf
    if [[ ! -f "${DOTFILES_VENDOR_DIR}/fzf/bin/fzf" ]]; then
        echo "Installing FZF binary."
        updater-print-info "Upgrading fzf."
        "${DOTFILES_VENDOR_DIR}/fzf/install" --bin --no-update-rc --no-key-bindings --no-completion
    else
        echo "FZF is up to date."
    fi

    # nvm is sourced by .zshrc.

    # st
    updater-pushd "${DOTFILES_VENDOR_DIR}/st"
    updater-print-info "Upgrading suckless terminal."
    # If we don't remove config.h, then changes in config.def.h are not
    # generated to replace config.h.  config.h is not tracked by git and is
    # generated from config.def.h so we always want to replace it.
    rm -f config.h
    sudo make clean install
    updater-popd

    updater-popd
}

function update-dotfile-symlinks() {
    updater-print-info "Updating symlinks to ~/.dotfiles"
    if [[ "$(expr substr $(uname -s) 1 5)" == 'Linux' ]]; then
        rcup -t linux
    else
        rcup
    fi
}

function update-current-zsh() {
    updater-print-info "Updating current ZSH instance."
    reload-zshrc
}

function update-current-tmux() {
    if [[ -n "${TMUX}" ]]; then
        updater-print-info "Updating current Tmux instance."
        tmux source-file ~/.tmux.conf
			  tmux display-message -p "[From Tmux] Sourced .tmux.conf."
    fi
}

# Command to get this workstation synchronized with the latest changes dotfile
# changes.
function open-sesame() {
    updater-print-info "Welcome back! Lets get you up to speed..."
    if which-command "open-sesame-system" > /dev/null; then
        open-sesame-system
        echo
    else
        updater-print-info "No system specific function found."
    fi
    update-dotfiles
    echo
    update-dotfile-symlinks
    echo
    update-current-zsh
    echo
    update-current-tmux
    updater-print-success "  You're five-by-five, good-to-go.  "
}

# Create a patched consolas font.  The nerd-fonts repo should be on my
# 'consolas' branch
function make-consolas-nerd-font() {
    updater-print-info "Creating consolas nerd font."
    updater-pushd "${HOME}/prog/nerd-fonts"
    ./font-patcher --quiet --fontawesome --fontlinux --octicons --pomicons \
                   --powerline --powerlineextra \
                   --outputdir "${DOTFILES_DIR}/fonts" \
                   "${DOTFILES_DIR}/fonts/consola.ttf"
    "${DOTFILES_DIR}/fonts/install.sh"
    st zsh -c '. ~/.zshrc; test-fonts; $SHELL'
    updater-popd
}
