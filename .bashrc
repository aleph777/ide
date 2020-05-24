# -*-Shell-script-*-

# Append to the history file, don't overwrite it.
shopt -s histappend

# Don't know why this is needed
enable kill

# aliases are resolved recursively
#   alias hello="echo Hello"
#   alias world='hello World!'
#
alias perl='perl -Mv5.10'
alias avg='perl -e '\''use List::Util qw(sum);say sum(@ARGV)/@ARGV;'\'''
alias say='perl -e "say $_ for @ARGV"'

alias clt='printf "\033c"'

alias grep='grep -P'
alias hg='history | grep '

alias lsc='TERM=ansi ls --color=always'
alias lsf='ls -F'

alias psfind='ps u -C'

alias undo-commit='git reset --soft HEAD~1'

alias up='sudo apt update && sudo apt upgrade'
alias sshfs='sudo sshfs -o allow_other ubuntu@10.46.28.59:/home/ubuntu pi'

# Ignore these commands
export HISTIGNORE="ls:[bf]g:exit"
export HISTCONTROL=ignoredups
export PAGER='/usr/bin/less -ins'

export IDE="$HOME/ide"

export PERLLIB="$IDE/lib:$IDE/local/lib"
export PERL5LIB="$PERLLIB"

export CLANG=/usr/local/clang+llvm-8.0.0-x86_64-linux-gnu-ubuntu-16.04
export CMAKE=/usr/local/cmake-3.13.4-Linux-x86_64

export CLANGBIN=$CLANG/bin
export CLANGLIB=$CLANG/lib
export CMAKEBIN=$CMAKE/bin
export SNAPBIN=/snap/bin

alias cmake="$CMAKEBIN/cmake"

export DEFAULTPATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
export EXTRAPATH=/usr/local/go/bin:/usr/gnu/bin:/usr/X11/bin
export PATH=$($HOME/bin/clean-path $IDE/local/bin $IDE/local/homebin $IDE/bin $DEFAULTPATH $PATH $SNAPBIN $EXTRAPATH)

export LD_LIBRARY_PATH=$CLANGLIB
export MANPATH=$($HOME/bin/clean-path /usr/local/share/man /usr/share/man $MANPATH)

export BRANCH='git rev-parse --abbrev-ref HEAD'
export EMACSVERSION=$(perl -e '$x=qx(emacs --version);$v=($x=~/(\d{2}(?:\.\d{1,2}){1,2})/)[0];say $v')
export LISP="/usr/local/share/emacs/${EMACSVERSION}/lisp"

export SHOW_CPP_INCLUDES='g++ -E -Wp,-v -xc /dev/null'
export SHOW_LD_PATHS="ld --verbose | grep SEARCH_DIR | tr -s ' ;' \\012"

#export PS1='\# [\h] \W> '

#
# Emacs
#
export EDITOR='emacsclient -n -c'
export EMACSARGS='--no-site-file --no-site-lisp --no-splash --no-loadup --no-x-resources'

#
# git
#
#alias "git-reset-from-remote='git checkout origin/develop -- '"

alias emacs="emacs $EMACSARGS"
alias emacs28="~/emacs/src/emacs $EMACSARGS"
alias emacsclient='/usr/local/bin/emacsclient -n -c'
alias emacsdaemon='emacs --daemon'
alias emacsstop="/usr/local/bin/emacsclient --eval '(kill-emacs)'"

export COLUMNS=108

# Signal Name	Signal Number	Description
# SIGHUP	1	Hang up detected on controlling terminal or death of controlling process
# SIGINT	2	Issued if the user sends an interrupt signal (Ctrl + C)
# SIGQUIT	3	Issued if the user sends a quit signal (Ctrl + D)
# SIGFPE	8	Issued if an illegal mathematical operation is attempted
# SIGKILL	9	If a process gets this signal it must quit immediately and will not perform any clean-up operations
# SIGALRM	14	Alarm clock signal (used for timers)
# SIGTERM	15	Software termination signal (sent by kill by default)

# BLUE   = [34m$(1)[0m
# CYAN   = [36m$(1)[0m
# GREEN  = [32m$(1)[0m
# PURPLE = [35m$(1)[0m
# RED    = [31m$(1)[0m
# YELLOW = [33m$(1)[0m

# BOLD_BLUE   = [34m[1m$(1)[0m
# BOLD_CYAN   = [36m[1m$(1)[0m
# BOLD_GREEN  = [32m[1m$(1)[0m
# BOLD_PURPLE = [35m[1m$(1)[0m
# BOLD_RED    = [31m[1m$(1)[0m
# BOLD_YELLOW = [33m[1m$(1)[0m

                                sub printGreenBold  { print STDERR "[32m[1m$_[0m" for @_; }
# sub printPurpleBold { print STDERR "[35m[1m$_[0m" for @_; }
# sub printYellowBold { print STDERR "[33m[1m$_[0m" for @_; }
# sub sayGreenBold    { print STDERR "[32m[1m$_[0m" for @_; }
# sub sayPurpleBold   { say   STDERR "[35m[1m$_[0m" for @_; }
# sub sayYellowBold   { say   STDERR "[33m[1m$_[0m" for @_; }
# RED="\[\033[0;31m\]"

LIGHT_RED="\[\033[1;31m\]"
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

NO_COLOUR="\[\033[0m\]"
#
# OLD STUFF (you never know)
#
# if [ $CLEARCASE_ROOT ];
# then
#   export PS1='\# [\h($(basename $CLEARCASE_ROOT))] \W> ';
# fi
export PS1=${LIGHT_RED}'\# [\h] \W> '${NO_COLOUR}
