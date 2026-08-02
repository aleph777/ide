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
alias say='perl -e "say $_ for @ARGV"'
alias sum="perl -e 'my \$s=0;\$s+=\$_ for @ARGV;say \$s'"
alias avg="perl -e 'my \$s=0;\$s+=\$_ for @ARGV;say \$s/@ARGV'"
alias prod="perl -e 'my \$p=1;\$p*=\$_ for @ARGV;say \$p'"
alias v4='v4l2-ctl'
alias arch='uname -m'
alias ten="cd ~/Workspace/tenbeauty/"

alias flake8='flake8 --ignore E221,E303,E501'

# apt
#
alias up='sudo apt update && sudo apt upgrade -y'

# Git
#
alias undo-commit='git reset --soft HEAD~1'

export BRANCH='git rev-parse --abbrev-ref HEAD'

# Bash
#
alias clt='printf "\033c"'
alias lsc='TERM=ansi ls --color=always'
alias lsf='ls -F'

alias grep='grep -P'
alias hg='history | grep '
alias psfind='ps u -C'

alias myinet='echo $(curl -s https://api.ipify.org)'
alias mylan='ifconfig | grep -A1 BROADCAST,RUNNING,MULTICAST | grep inet | cut -d" " -f10'
# alias mywlan='iw dev wlan0 link'
alias mywlan='iw dev wlp0s20f3 link'
alias aslan='sudo arp-scan --localnet'
alias gtag='git for-each-ref --sort=creatordate --format "%(refname)" refs/tags | cut -d/ -f3'

# these are a handy reference
#
# alias sshfs='sudo sshfs -o allow_other ubuntu@10.46.28.59:/home/ubuntu pi'
# ssh-copy-id ubuntu@192.168.1.253

# setting up video resolution
#
# export CVT=$(cvt 3840 2160 | cut -d" " -f2- | tail -1)
# export CVT_MODE_NAME=$(echo $CVT | cut -d" " -f1)
export CVT='"3840x2160" 712.75 3840 4160 4576 5312 2160 2163 2168 2237 -hsync +vsync'
export CVT_MODE_NAME='"3840x2160"'
export CVT_MONITOR='Virtual1'
export CVT_NEWMODE="xrandr --newmode $CVT"
export CVT_ADDMODE="xrandr --addmode $CVT_MONITOR $CVT_MODE_NAME"
export CVT_SET="$CVT_NEWMODE && $CVT_ADDMODE"

export PERLLIB="$IDE/lib:$IDE/local/lib"
export PERL5LIB="$PERLLIB"
export PYTHONPATH="$IDE/lib/python"

# Emacs
#
export EDITOR='nano'

export EMACSDIR=$HOME/emacs
export EMACSBIN=$EMACSDIR/src/emacs
export EMACSARGS='--no-site-file --no-site-lisp --no-splash --no-loadup --no-x-resources'

alias emacs="$EMACSBIN $EMACSARGS"
alias qmacs="$EMACSBIN -Q -l $HOME/early-init.el -l $HOME/.emacs.el"
alias emacsclient="$EMACSDIR/lib-src/emacsclient -n -c"
alias emacsdaemon="$EMACSBIN --daemon"
alias emacsstop="emacsclient --eval '(kill-emacs)'"
alias emacsclone='git clone https://git.savannah.gnu.org/git/emacs.git'
alias emacsclone31='git clone -b emacs-31 https://github.com/emacs-mirror/emacs.git'

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
    export THIS_ARCH=$(uname -m)
fi
if [[ -z "$THIS_ID" ]]; then
    export THIS_ID=$(grep '^ID=' /etc/os-release | cut -d= -f2)
fi
if [[ -z "$THIS_VERSIONID" ]]; then
    export THIS_VERSION_ID=$(grep '^VERSION_ID=' /etc/os-release | cut -d= -f2 | cut -d'"' -f2)
fi

BOLD=$(tput bold)
RESET=$(tput sgr0)

BLACK=$(tput setaf 0)
RED=$(tput setaf 1)
GREEN=$(tput setaf 2)
YELLOW=$(tput setaf 3)
BLUE=$(tput setaf 4)
MAGENTA=$(tput setaf 5)
CYAN=$(tput setaf 6)
WHITE=$(tput setaf 7)

if [[ "$THIS_ARCH" = "x86_64" ]]; then
    PROMPT_COLOR=${BOLD}${GREEN}
else
    PROMPT_COLOR=${BOLD}${YELLOW}
fi
export PS1='${PROMPT_COLOR}\h[${THIS_ARCH} ${THIS_ID} ${THIS_VERSION_ID}] \W> $RESET'

# ==============================================================================

export LOCAL_INSTALL_DIR=$($CLEANPATH $LOCAL_INSTALL_DIR:/home/fontaine/.local)
export PATH=$($CLEANPATH $PATH:$LOCAL_INSTALL_DIR/bin)
export LD_LIBRARY_PATH=$($CLEANPATH $LD_LIBRARY_PATH:$LOCAL_INSTALL_DIR/lib:/usr/local/lib)
# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
# __conda_setup="$('/home/fontaine/miniconda3/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
# if [ $? -eq 0 ]; then
#     eval "$__conda_setup"
# else
#     if [ -f "/home/fontaine/miniconda3/etc/profile.d/conda.sh" ]; then
#         . "/home/fontaine/miniconda3/etc/profile.d/conda.sh"
#     else
#         export PATH="/home/fontaine/miniconda3/bin:$PATH"
#     fi
# fi
# unset __conda_setup
# <<< conda initialize <<<
