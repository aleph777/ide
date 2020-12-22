# -*-Shell-script-*-

if [[ -e $HOME/.bashrc_original ]]; then
    source $HOME/.bashrc_original
fi

# Signal Name	Signal Number	Description
# SIGHUP	1	Hang up detected on controlling terminal or death of controlling process
# SIGINT	2	Issued if the user sends an interrupt signal (Ctrl + C)
# SIGQUIT	3	Issued if the user sends a quit signal (Ctrl + D)
# SIGFPE	8	Issued if an illegal mathematical operation is attempted
# SIGKILL	9	If a process gets this signal it must quit immediately and will not perform any clean-up operations
# SIGALRM	14	Alarm clock signal (used for timers)
# SIGTERM	15	Software termination signal (sent by kill by default)

# Append to the history file, don't overwrite it.
#
shopt -s histappend
export HISTCONTROL=ignoreboth
export HISTIGNORE="bg:cd:exit:fg:hg:history:ls"
export HISTSIZE=999
export PROMPT_COMMAND="history -a; history -n"

# Don't know why this is needed
#
enable kill

export IDE="$HOME/ide"

# aliases are resolved recursively
#   alias hello="echo Hello"
#   alias world='hello World!'
#
alias perl='perl -Mv5.10'
alias avg='perl -e '\''use List::Util qw(sum);say sum(@ARGV)/@ARGV;'\'''
alias say='perl -e "say $_ for @ARGV"'

export PERLLIB="$IDE/lib:$IDE/local/lib"
export PERL5LIB="$PERLLIB"

# apt
#
alias up='sudo apt update && sudo apt -y upgrade'

# Git
#
alias undo-commit='git reset --soft HEAD~1'
# alias "git-reset-from-remote='git checkout origin/develop -- '"
export BRANCH='git rev-parse --abbrev-ref HEAD'

# Bash
#
alias clt='printf "\033c"'
alias grep='grep -P'
alias hg='history | grep '
alias lsc='TERM=ansi ls --color=always'
alias lsf='ls -F'
alias psfind='ps u -C'

# these are a handy reference
#
# alias sshfs='sudo sshfs -o allow_other ubuntu@10.46.28.59:/home/ubuntu pi'
# ssh-copy-id ubuntu@192.168.1.253

# Emacs
#
export EDITOR='nano'
export EMACSARGS='--no-site-file --no-site-lisp --no-splash --no-loadup --no-x-resources'
export EMACSBIN=~/emacs/src/emacs
#export EMACSVERSION=$(emacs --version | grep '\d$' | cut -d' ' -f3)
export LISP="~/emacs/lisp"
alias emacs="$EMACSBIN $EMACSARGS"
# alias emacsclient='/usr/local/bin/emacsclient -n -c'
# alias emacsdaemon='emacs --daemon'
# alias emacsstop="/usr/local/bin/emacsclient --eval '(kill-emacs)'"

# Ignore these commands
#
export PAGER='/usr/bin/less -ins'
export COLUMNS=108

# export CLANG=/usr/local/clang+llvm-8.0.0-x86_64-linux-gnu-ubuntu-16.04
# export CMAKE=/usr/local/cmake-3.13.4-Linux-x86_64

# export CLANGBIN=$CLANG/bin
# export CLANGLIB=$CLANG/lib
# export CMAKEBIN=$CMAKE/bin

# alias cmake="$CMAKEBIN/cmake"

if [[ -z "$IP" ]]; then
    export IP=$(ifconfig | head -2 | grep inet | cut -d' ' -f10)
fi

export DEFAULTPATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
export EXTRAPATH=/usr/local/go/bin:/usr/gnu/bin:/usr/X11/bin
export SNAPBIN=/snap/bin
export PATH=$($HOME/bin/clean-path $IDE/local/bin $IDE/local/homebin $IDE/bin $DEFAULTPATH $PATH $SNAPBIN $EXTRAPATH)

# export LD_LIBRARY_PATH=$CLANGLIB
export MANPATH=$($HOME/bin/clean-path /usr/local/share/man /usr/share/man $MANPATH)

export SHOW_CPP_INCLUDES='g++ -E -Wp,-v -xc /dev/null'
export SHOW_LD_PATHS="ld --verbose | grep SEARCH_DIR | tr -s ' ;' \\012"

if [[ -z "$THIS_ARCH" ]]; then
    export THIS_ARCH=$(uname -m)
fi
if [[ -z "$THIS_ID" ]]; then
    export THIS_ID=$(grep '^ID=' /etc/os-release | cut -d= -f2)
fi
if [[ -z "$THIS_VERSIONID" ]]; then
    export THIS_VERSION_ID=$(grep '^VERSION_ID=' /etc/os-release | cut -d= -f2 | cut -d'"' -f2)
fi

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

# sub printGreenBold  { print STDERR "[32m[1m$_[0m" for @_; }
# sub printPurpleBold { print STDERR "[35m[1m$_[0m" for @_; }
# sub printYellowBold { print STDERR "[33m[1m$_[0m" for @_; }
# sub sayGreenBold    { print STDERR "[32m[1m$_[0m" for @_; }
# sub sayPurpleBold   { say   STDERR "[35m[1m$_[0m" for @_; }
# sub sayYellowBold   { say   STDERR "[33m[1m$_[0m" for @_; }
# RED="\[\033[0;31m\]"

# LIGHT_RED="\[\033[1;31m\]"
# ORANGE='\033[0;33m'
# YELLOW="\[\033[1;33m\]"
# GREEN="\[\033[0;32m\]"
LIGHT_GREEN="\[\033[1;32m\]"
# CYAN="\[\033[0;36m\]"
# LIGHT_CYAN="\[\033[1;36m\]"
# BLUE="\[\033[0;34m\]"
LIGHT_BLUE="\[\033[1;34m\]"
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
# export PS1=${LIGHT_GREEN}'\# [\h] \W> '${NO_COLOUR}

if [[ "$THIS_ARCH" = "x86_64" ]]; then
    PROMPT_COLOR=${LIGHT_GREEN}
else
    PROMPT_COLOR=${LIGHT_BLUE}
fi
export PS1=${PROMPT_COLOR}'\h[${THIS_ARCH} ${THIS_ID} ${THIS_VERSION_ID}] \W> '${NO_COLOUR}
