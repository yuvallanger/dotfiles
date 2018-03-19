#!/bin/bash

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors
    if [[ $? == 0 ]]; then
        eval "$(dircolors -b ~/.dircolors)";
    else
        eval "$(dircolors -b)"
    fi
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

alias g=git
alias gl='g l'
alias gs='g s'
alias gc='g c'
alias gd='g d'

alias e="emacsclient -a= -c"

alias hebdate="hdate --hebrew|cut -f 3 -d ','|grep -v '^$'"
alias emptify="truncate -c -s 0"

function pip3fr () { pip3 freeze --user; }
function pip3i () { pip3 install --user --upgrade $@; }
function pip3up () { pip3i "$( pip3fr | sed 's#==.*##' )"; }
function pip3un () { pip3 uninstall "$*"; }

# http://cs-syd.eu/posts/2015-06-21-gtd-with-taskwarrior-part-2-collection.html
function inn () { task add +in $@; }
function tickle () {
        deadline="$1"
        shift
        inn +tickle wait:"$deadline" "$*"
}
alias tick=tickle
alias think='tickle +1d'

alias tracli="transmission-remote-cli"
alias trarem="transmission-remote"

function ppgrep ()
{
        if [[ $1 == "" ]];
        then
                PERCOL=percol
        else
                PERCOL="percol --query $1"
        fi
        ps aux | eval "$PERCOL" | awk '{ print $2 }'
}

function ppkill ()
{
        if [[ $1 =~ ^- ]];
        then
                QUERY=""
        else
                QUERY=$1
                [[ $# -gt 0 ]] && shift
        fi
        ppgrep $QUERY | xargs kill $*
}
