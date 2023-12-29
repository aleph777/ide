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
#
export HISTCONTROL=ignoreboth:erasedups
export HISTIGNORE="bg:cd:exit:fg:hg:history:ls"
export HISTSIZE=999
# export PROMPT_COMMAND="history -a; history -n"

# Don't know why this is needed
#
enable kill

export IDE="$HOME/ide"

# aliases are resolved recursively
#   alias hello="echo Hello"
#   alias world='hello World!'
#
alias perl='perl -MModern::Perl'
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
alias inetip='echo $(curl -s https://api.ipify.org)'
alias localip='ip a'

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
    export IP=$(ifconfig | grep -A1 BROADCAST,RUNNING,MULTICAST | grep inet | cut -d' ' -f10)
fi

export DEFAULTPATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
export EXTRAPATH=/usr/local/go/bin:/usr/gnu/bin:/usr/X11/bin
export HOMEBIN=$IDE/bin
export HOMEPATH=$HOME/.local/bin:$IDE/local/bin:$IDE/local/homebin:$HOMEBIN
export OPTPATH=
export SNAPBIN=/snap/bin
export CLEANPATH=$HOMEBIN/clean-path
export PATH=$($CLEANPATH $OPTPATH $HOMEPATH $DEFAULTPATH $PATH $SNAPBIN $EXTRAPATH)

export MANPATH=$($CLEANPATH /usr/local/share/man /usr/share/man $MANPATH)

export SHOW_CPP_INCLUDES='g++ -E -Wp,-v -xc /dev/null'
export SHOW_LD_PATHS="ld --verbose | grep SEARCH_DIR | tr -s ' ;' \\012"

if [[ -z "$THIS_ARCH" ]]; then
    export THIS_ARCH=$(arch)
fi
if [[ -z "$THIS_ID" ]]; then
    export THIS_ID=$(grep '^ID=' /etc/os-release | cut -d= -f2)
fi
if [[ -z "$THIS_VERSIONID" ]]; then
    export THIS_VERSION_ID=$(grep '^VERSION_ID=' /etc/os-release | cut -d= -f2 | cut -d'"' -f2)
fi

BOLD=$(tput bold)
NORMAL=$(tput sgr0)

BLACK=$(tput setaf 0)
RED=$(tput setaf 1)
GREEN=$(tput setaf 2)
YELLOW=$(tput setaf 3)
BLUE=$(tput setaf 4)
MAGENTA=$(tput setaf 5)
CYAN=$(tput setaf 6)
WHITE=$(tput setaf 7)

# OLD STUFF (you never know)
#
# if [ $CLEARCASE_ROOT ];
# then
#   export PS1='\# [\h($(basename $CLEARCASE_ROOT))] \W> ';
# fi
# export PS1=${LIGHT_GREEN}'\# [\h] \W> '${NO_COLOUR}

if [[ "$THIS_ARCH" = "x86_64" ]]; then
    PROMPT_COLOR=${BOLD}${GREEN}
else
    PROMPT_COLOR=${BOLD}${BLUE}
fi
export PS1=${PROMPT_COLOR}'\h[${THIS_ARCH} ${THIS_ID} ${THIS_VERSION_ID}] \W> '$NORMAL

# ==============================================================================

export LOCAL_INSTALL_DIR=$($CLEANPATH $LOCAL_INSTALL_DIR:/home/fontaine/.local)
export PATH=$($CLEANPATH $PATH:$LOCAL_INSTALL_DIR/bin)
export LD_LIBRARY_PATH=$($CLEANPATH $LD_LIBRARY_PATH:$LOCAL_INSTALL_DIR/lib:/usr/local/lib)

ppverbosefunc() {
    cd ~/Workspace/tenbeauty/build/path_planner_cpp/src
    export GLOG_alsologtostderr=true
    export GLOG_stderrthreshold=0
    export GLOG_v="$1"
}

export CMAKE_PREFIX_PATH=$($CLEANPATH $CMAKE_PREFIX_PATH:/usr/aarch64-linux-gnu)
export AARCH64GCC_DIR=/usr
