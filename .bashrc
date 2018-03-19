#!/bin/bash

# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples


if [ -d "$HOME/.local/bin" ] ; then
        export PATH="$HOME/.local/bin:$PATH"
fi

if [ -d "$HOME/bin" ] ; then
        export PATH="$HOME/bin:$PATH"
fi

# If not running interactively, don't do anything
case $- in
    ,*i*) ;;
    ,*) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
[ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ] && debian_chroot="$(cat /etc/debian_chroot)"

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color|*-256color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
force_color_prompt=""

if [ -n "${force_color_prompt}" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
        # We have color support; assume it's compliant with Ecma-48
        # (ISO/IEC-6429). (Lack of such support is extremely rare, and such
        # a case would tend to support setf rather than setaf.)
        color_prompt=yes
    else
        color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
,*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    if [ -r ~/.dircolors ];
    then
        eval "$(dircolors -b ~/.dircolors)"
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

# colored GCC warnings and errors
#export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$( [ $? = 0 ] && echo terminal || echo error ; )" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ];
then
    # shellcheck source=.bash_aliases
    source "$HOME/.bash_aliases"
fi

export PYTHONPATH="$HOME/.local/lib/python3.6:$PYTHONPATH"

if [ -d "$HOME/gopath" ] ; then
        export GOPATH="$HOME/gopath"
        export PATH="$GOPATH/bin:$PATH"
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    source /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    source /etc/bash_completion
  fi
fi
if [ -r ~/.byobu/prompt ];
then
    # shellcheck source=.byobu/prompt
    source "$HOME/.byobu/prompt"   #byobu-prompt#
fi

# For pipenv and virtualenvwrapper
if [ -d "$HOME/.virtualenvs" ]; then
        export WORKON_HOME="$HOME/.virtualenvs"
fi
#export VIRTUALENVWRAPPER_PYTHON="/usr/bin/python3.6"
if [ -f "$HOME/.local/binvirtualenvwrapper_lazy.sh" ]; then
    # shellcheck source=.local/bin/virtualenvwrapper_lazy.sh
    source "$HOME/.local/bin/virtualenvwrapper_lazy.sh"
fi

if [ -f "$HOME/.local/bin/find_pycompletion.sh" ]; then
    # shellcheck source=.local/bin/find_pycompletion.sh
    source "$(find_pycompletion.sh)"
fi

# https://unix.stackexchange.com/questions/72086/ctrl-s-hang-terminal-emulator
stty -ixon

printf "And now for something completely different:\n\n"
fortune -c -a | sed 's/^/    /'
printf "\n"

# [[file:~/foo/dotfiles/dotfiles.org::*bashrc][bashrc:1]]
#!/bin/bash

export LC_ALL=C.UTF-8
export LANG=C.UTF-8
export LANGUAGE=C.UTF-8

export ALTERNATE_EDITOR=""

# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

if [ -d "$HOME/.local/bin" ] ; then
        export PATH="$HOME/.local/bin:$PATH"
fi

if [ -d "$HOME/bin" ] ; then
        export PATH="$HOME/bin:$PATH"
fi

# If not running interactively, don't do anything
case $- in
    *i*) ;;
    *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
[ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ] && debian_chroot="$(cat /etc/debian_chroot)"

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color|*-256color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
force_color_prompt=""

if [ -n "${force_color_prompt}" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
        # We have color support; assume it's compliant with Ecma-48
        # (ISO/IEC-6429). (Lack of such support is extremely rare, and such
        # a case would tend to support setf rather than setaf.)
        color_prompt=yes
    else
        color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    if [ -r ~/.dircolors ];
    then
        eval "$(dircolors -b ~/.dircolors)"
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

# colored GCC warnings and errors
#export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$( [ $? = 0 ] && echo terminal || echo error ; )" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ];
then
    # shellcheck source=.bash_aliases
    source "$HOME/.bash_aliases"
fi

export PYTHONPATH="$HOME/.local/lib/python3.6:$PYTHONPATH"

if [ -d "$HOME/gopath" ] ; then
        export GOPATH="$HOME/gopath"
        export PATH="$GOPATH/bin:$PATH"
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    source /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    source /etc/bash_completion
  fi
fi
if [ -r ~/.byobu/prompt ];
then
    # shellcheck source=.byobu/prompt
    source "$HOME/.byobu/prompt"   #byobu-prompt#
fi

# For pipenv and virtualenvwrapper
if [ -d "$HOME/.virtualenvs" ]; then
        export WORKON_HOME="$HOME/.virtualenvs"
fi

#export VIRTUALENVWRAPPER_PYTHON="/usr/bin/python3.6"
if [ -f "$HOME/.local/binvirtualenvwrapper_lazy.sh" ]; then
    # shellcheck source=.local/bin/virtualenvwrapper_lazy.sh
    source "$HOME/.local/bin/virtualenvwrapper_lazy.sh"
fi

_pipenv_completion() {
    local IFS=$'\t'
    COMPREPLY=( $( env COMP_WORDS="${COMP_WORDS[*]}" \
                   COMP_CWORD=$COMP_CWORD \
                   _PIPENV_COMPLETE=complete-bash "$1" ) )
    return 0
}

complete -F _pipenv_completion -o default pipenv

if [ -f "$HOME/.local/bin/find_pycompletion.sh" ]; then
    # shellcheck source=.local/bin/find_pycompletion.sh
    source "$(find_pycompletion.sh)"
fi

# https://unix.stackexchange.com/questions/72086/ctrl-s-hang-terminal-emulator
stty -ixon

printf "And now for something completely different:\n\n"

fortune -c -a

printf "\n"
# bashrc:1 ends here

# pip bash completion start
_pip_completion()
{
    COMPREPLY=( $( COMP_WORDS="${COMP_WORDS[*]}" \
                   COMP_CWORD=$COMP_CWORD \
                   PIP_AUTO_COMPLETE=1 $1 ) )
}


complete -o default -F _pip_completion pip
# pip bash completion end

if [ "$TILIX_ID" ] || [ "$VTE_VERSION" ] ; then source /etc/profile.d/vte.sh; fi # Ubuntu Budgie END
