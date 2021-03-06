#
# ~/.bashrc
#

[ -z "$PS1" ] && return

export VIRTUAL_ENV_DISABLE_PROMPT=1

export EDITOR=vim

setterm -blength 0

if [ -d "$HOME/bin" ]; then
    export PATH=$HOME/bin:$PATH
fi

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

export PATH=$PATH:$HOME/.gem/ruby/1.9.1/bin

. $HOME/.venv/bin/activate

eval `ssh-agent` > /dev/null
ssh-add 2> /dev/null

HISTCONTROL=ignoreboth
shopt -s histappend
shopt -s checkwinsize
 
if [ -f /usr/share/bash-completion/bash_completion ] && ! shopt -oq posix; then
    . /usr/share/bash-completion/bash_completion
fi

