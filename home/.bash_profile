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
    export PATH=`echo -n $PATH | awk -v RS=: -v ORS=: '$0 != "'$1'"' | sed 's/:$//'`;
}

# put /usr/local/bin before /usr/bin
path_remove "/usr/local/bin"

export PATH="/usr/local/bin:$PATH"
export PATH="/usr/local/sbin:$PATH"
export PATH="$HOME/bin:$PATH"
export PATH="$HOME/.rvm/bin:$PATH"
export PATH='.:$PATH'


if [ -f ~/.bashrc ]; then
   source ~/.bashrc
fi
[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx

