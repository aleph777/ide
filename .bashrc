# -*-Shell-script-*-

# Append to the history file, don't overwrite it.
shopt -s histappend

alias lsf='ls -F'
alias ls='TERM=ansi ls --color=always'
alias avg='perl -e '\''use List::Util qw(sum);CORE::say sum(@ARGV)/@ARGV;'\'''
alias perl='perl -Mv5.10'
alias psfind='ps u -C'

# Ignore these commands
export HISTIGNORE="ls:[bf]g:exit"
export HISTCONTROL=ignoredups
export PAGER='/usr/bin/less -ins'

export PERLLIB="$HOME/lib:$HOME/local/lib"

export DEFAULTPATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
export EXTRAPATH=/usr/local/go/bin:/usr/gnu/bin:/usr/X11/bin
export PATH=$($HOME/bin/clean-path $HOME/local/bin $HOME/local/homebin $HOME/bin $DEFAULTPATH $PATH $EXTRAPATH)

export MANPATH=$($HOME/bin/clean-path /usr/local/share/man /usr/share/man $MANPATH)

export EMACSVERSION=$(perl -e '$x=qx(emacs --version);$v=($x=~/(\d{2}(?:\.\d{1,2}){1,2})/)[0];say $v')
export LISP="/usr/local/share/emacs/${EMACSVERSION}/lisp"

export PS1='\# [\h] \W> '

#
# Emacs
#
export EDITOR='emacsclient -n -c'
export EMACSARGS='--no-site-file --no-site-lisp --no-splash --no-loadup --no-x-resources'

alias emacs="emacs $EMACSARGS"
alias emacsclient='/usr/local/bin/emacsclient -n -c'
alias emacsdaemon='emacs --daemon'
alias emacsstop="/usr/local/bin/emacsclient --eval '(kill-emacs)'"


export COLUMNS=108

# RED="\[\033[0;31m\]"
# LIGHT_RED="\[\033[1;31m\]"
# ORANGE='\033[0;33m'
# YELLOW="\[\033[1;33m\]"
# GREEN="\[\033[0;32m\]"
# LIGHT_GREEN="\[\033[1;32m\]"
# CYAN="\[\033[0;36m\]"
# LIGHT_CYAN="\[\033[1;36m\]"
# BLUE="\[\033[0;34m\]"
# LIGHT_BLUE="\[\033[1;34m\]"
# PURPLE="\[\033[0;35m\]"

# WHITE='\e[0;37m'
# LIGHT_GRAY="\[\033[0;37m\]"
# GRAY="\[\033[1;30m\]"
# BLACK="\[\033[0;30m\]"

# NO_COLOUR="\[\033[0m\]"
#
# OLD STUFF (you never know)
#
# if [ $CLEARCASE_ROOT ];
# then
#   export PS1='\# [\h($(basename $CLEARCASE_ROOT))] \W> ';
# fi
