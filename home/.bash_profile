echo "loading .bash_profile"
pathmunge () {
    if ! echo $PATH | grep -Eq "(^|:)$1($|:)" ; then
        if [ "$2" = "after" ] ; then
            PATH=$PATH:$1
        else
            PATH=$1:$PATH
        fi
    fi
}
path_remove ()  {
    PATH=`echo -n $PATH | awk -v RS=: -v ORS=: '$0 != "'$1'"' | sed 's/:$//'`;
}

# put /usr/local/bin before /usr/bin
path_remove "/usr/local/bin"

PATH="/usr/local/bin:$PATH"
PATH="/usr/local/sbin:$PATH"
PATH="$HOME/bin:$PATH"
PATH="$HOME/.rvm/bin:$PATH"
PATH="$PATH:$HOME/.cabal/bin"
PATH=".:$PATH"
PATH="$PATH:/usr/share/texmf-dist/scripts/texlive"



if [ -f ~/.bashrc ]; then
   source ~/.bashrc
fi

[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx

