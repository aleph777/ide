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
# shopt -s histappend
export HISTCONTROL=ignoreboth:erasedups
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
alias sum="perl -e 'use List::Util qw(sum); say sum(@ARGV);'"

alias ten="cd ~/Workspace/tenbeauty/"

alias flake8='flake8 --ignore E221,E303,E501'

export PERLLIB="$IDE/lib:$IDE/local/lib"
export PERL5LIB="$PERLLIB"
export PYTHONPATH="$IDE/lib/python"

# apt
#
alias up='sudo apt update && sudo apt upgrade'

# Git
#
alias undo-commit='git reset --soft HEAD~1'

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

# setting up video resolution
#
alias set1920x1080='xrandr --newmode $(cvt 1920 1080 | cut -d" " -f2- | tail -1) && xrandr --addmode Virtual1 "1920x1080_60.00"'
alias newmode='xrandr --newmode "1920x1080_60.00"  173.00  1920 2048 2248 2576  1080 1083 1088 1120 -hsync +vsync'
alias addmode='xrandr --addmode Virtual1 "1920x1080_60.00"'

# Emacs
#
export EDITOR='nano'

export EMACSDIR=$HOME/emacs
export EMACSBIN=$EMACSDIR/src/emacs
export EMACSARGS='--no-site-file --no-site-lisp --no-splash --no-loadup --no-x-resources'

alias emacs="$EMACSBIN $EMACSARGS"
alias emacsclient="$EMACSDIR/lib-src/emacsclient -n -c"
alias emacsdaemon='emacs --daemon'
alias emacsstop="emacsclient --eval '(kill-emacs)'"
alias emacsclone='git clone https://git.savannah.gnu.org/git/emacs.git'

# Ignore these commands
#
export PAGER='/usr/bin/less -ins'
export COLUMNS=108

# export CLANG=/usr/local/clang+llvm-8.0.0-x86_64-linux-gnu-ubuntu-16.04
# export CMAKE=/usr/local/cmake-3.13.4-Linux-x86_64

# export CLANGBIN=$CLANG/bin
# export CLANGLIB=$CLANG/lib
# export CMAKEBIN=$CMAKE/bin
# export LD_LIBRARY_PATH=$CLANGLIB

# alias cmake="$CMAKEBIN/cmake"

if [[ -z "$IP" ]]; then
    export IP=$(ifconfig | head -2 | grep inet | cut -d' ' -f10)
fi

export DEFAULTPATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
export EXTRAPATH=/usr/local/go/bin:/usr/gnu/bin:/usr/X11/bin
export HOMEPATH=$HOME/.local/bin:$IDE/local/bin:$IDE/local/homebin:$IDE/bin
export OPTPATH=$NODEJSDIR/bin:$ANDROIDDIR/bin
export SNAPBIN=/snap/bin
export PATH=$($HOME/bin/clean-path $OPTPATH $HOME/cinco/bin $HOMEPATH $DEFAULTPATH $PATH $SNAPBIN $EXTRAPATH)

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

BOLD="\[\033[1;"
NORMAL="\[\033[0;"

BLACK="30m\]"
RED="31m\]"
GREEN="32m\]"
YELLOW="33m\]"
BLUE="34m\]"
MAGENTA="35m\]"
CYAN="36m\]"
WHITE="37m\]"

BOLD_BLACK=${BOLD}${BLACK}
BOLD_RED=${BOLD}${RED}
BOLD_GREEN=${BOLD}${GREEN}
BOLD_YELLOW=${BOLD}${YELLOW}
BOLD_BLUE=${BOLD}${BLUE}
BOLD_MAGENTA=${BOLD}${MAGENTA}
BOLD_CYAN=${BOLD}${CYAN}
BOLD_WHITE=${BOLD}${WHITE}

NORMAL_BLACK=${NORMAL}${BLACK}
NORMAL_RED=${NORMAL}${RED}
NORMAL_GREEN=${NORMAL}${GREEN}
NORMAL_YELLOW=${NORMAL}${YELLOW}
NORMAL_BLUE=${NORMAL}${BLUE}
NORMAL_MAGENTA=${NORMAL}${MAGENTA}
NORMAL_CYAN=${NORMAL}${CYAN}
NORMAL_WHITE=${NORMAL}${WHITE}

NO_COLOR="\[\033[0m\]"

# OLD STUFF (you never know)
#
# if [ $CLEARCASE_ROOT ];
# then
#   export PS1='\# [\h($(basename $CLEARCASE_ROOT))] \W> ';
# fi
# export PS1=${LIGHT_GREEN}'\# [\h] \W> '${NO_COLOUR}

if [[ "$THIS_ARCH" = "x86_64" ]]; then
    PROMPT_COLOR=${BOLD_GREEN}
else
    PROMPT_COLOR=${BOLD_BLUE}
fi
export PS1=${PROMPT_COLOR}'\h[${THIS_ARCH} ${THIS_ID} ${THIS_VERSION_ID}] \W> '${NO_COLOR}

# ==============================================================================

export LOCAL_INSTALL_DIR=$(clean-path $LOCAL_INSTALL_DIR:/home/fontaine/.local)
export PATH=$(clean-path $PATH:$LOCAL_INSTALL_DIR/bin)
export LD_LIBRARY_PATH=$(clean-path $LD_LIBRARY_PATH:$LOCAL_INSTALL_DIR/lib:/usr/local/lib)

ppverbosefunc() {
    cd ~/Workspace/tenbeauty/build/path_planner_cpp/src
    export GLOG_alsologtostderr=true
    export GLOG_stderrthreshold=0
    export GLOG_v="$1"
}

export CMAKE_PREFIX_PATH=$(clean-path $CMAKE_PREFIX_PATH:/usr/aarch64-linux-gnu)
export AARCH64GCC_DIR=/usr
