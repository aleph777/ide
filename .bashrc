# -*-Bash-*-

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

if [ -z "$OS_NAME" ]; then
    LSB_CODENAME=$(lsb_release -cs | tr [:upper:] [:lower:])        # noble
    LSB_DESCRIPTION=$(lsb_release -ds | tr [:upper:] [:lower:])     # ubuntu 24.04.4 lts
    LSB_DISTRIBUTOR_ID=$(lsb_release -is | tr [:upper:] [:lower:])  # ubuntu
    LSB_RELEASE=$(lsb_release -rs)                                  # 24.04
    LSB_RELEASE_SHORT=$(echo $LSB_RELEASE | cut -d. -f1)            # 24
    LSB_RELEASE_LONG=$(echo $LSB_DESCRIPTION | cut -d' ' -f2)       # 24.04.4

    # human friendly names
    #
    export OS_NAME=$LSB_CODENAME                       # noble
    export OS_DISTRO=$LSB_DISTRIBUTOR_ID               # ubuntu
    export OS_VERSION=$LSB_RELEASE_SHORT               # 24
    export OS_RELEASE=$LSB_RELEASE                     # 24.04
    export OS_DISTRO_VERSION=${OS_DISTRO}${OS_VERSION} # ubuntu24
    export OS_DISTRO_LONG=$LSB_DESCRIPTION             # ubuntu 24.04.4 lts
fi

# Don't know why this is needed
#
enable kill

# figure out where we're living...
#
export IDE="$HOME/ide"

if [ ! -e "$IDE"  ]; then
    export IDE="$HOME/shared/ide"

    if [ ! -e "$IDE" ]; then
        # this better exist
        #
        export IDE="$HOME/share/ide"
    fi
fi
# try to get the paths right, but fail gracefully
#
export HOMEBIN="$IDE/bin"
export HOMEPATH="$HOME/.local/bin:$IDE/local/bin:$IDE/local/homebin:$HOME/.cargo/bin:$HOMEBIN"
export CLEANPATH="$HOMEBIN/clean-path"

export DEFAULTPATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
export EXTRAPATH=/usr/local/go/bin:/usr/gnu/bin:/usr/X11/bin
export OPTPATH=
export SNAPBIN=/snap/bin

export PATH="$OPTPATH:$HOMEPATH:$DEFAULTPATH:$PATH:$SNAPBIN:$EXTRAPATH"
export PERL5LIB="$IDE/lib:$IDE/local/lib:$HOME/perl5/lib/perl5"
export PYTHONPATH="$IDE/lib/python"
export MANPATH="/usr/local/share/man:/usr/share/man:$MANPATH"

$(which perl) -c $CLEANPATH 2>>/dev/null

if [ "$?" == 0 ]; then
    export PATH=$($CLEANPATH $PATH)
    export PERL5LIB=$($CLEANPATH $PERL5LIB)
    export PYTHONPATH=$($CLEANPATH $PYTHONPATH)
    export MANPATH=$($CLEANPATH $MANPATH)
else
    echo "$0: clean-path failure!!!"
fi

# Emacs
#
export EDITOR='nano'

export EMACSDIR=$HOME/emacs
export EMACSBIN=$EMACSDIR/src/emacs
export EMACSARGS='--no-site-file --no-site-lisp --no-splash --no-loadup --no-x-resources'
#
alias emacs="$EMACSBIN $EMACSARGS"
alias qmacs="$EMACSBIN -Q -l $HOME/early-init.el -l $HOME/.emacs.el"
alias stubmacs="$EMACSBIN -Q -l $IDE/elisp/stub.el"
alias emacsclient="$EMACSDIR/lib-src/emacsclient -n -c"
alias emacsdaemon='emacs --daemon'
alias emacsstop="emacsclient --eval '(kill-emacs)'"
alias emacsclone='git clone https://git.savannah.gnu.org/git/emacs.git'

# ------------------------------------------------------------------------------

# setting up video resolution
#
# export CVT=$(cvt 3840 2160 | cut -d" " -f2- | tail -1)
# export CVT_MODE_NAME=$(echo $CVT | cut -d" " -f1)
export CVT='"3840x2160" 712.75 3840 4160 4576 5312 2160 2163 2168 2237 -hsync +vsync'
export CVT_MODE_NAME='"3840x2160"'
export CVT_MONITOR='Virtual-1'

alias newmode="xrandr --newmode $CVT"
alias addmode="xrandr --addmode $CVT_MONITOR $CVT_MODE_NAME"
# ------------------------------------------------------------------------------

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
alias up='sudo apt update && sudo apt upgrade'

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
# alias set1920x1080='xrandr --newmode $(cvt 1920 1080 | cut -d" " -f2- | tail -1) && xrandr --addmode Virtual1 "1920x1080_60.00"'
# alias newmode='xrandr --newmode "1920x1080_60.00"  173.00  1920 2048 2248 2576  1080 1083 1088 1120 -hsync +vsync'
# alias addmode='xrandr --addmode Virtual1 "1920x1080_60.00"'

# alias gcc -xc -E -v - < /dev/null 2>&1 | sed -n '/#include.*search starts here:/,/End of search list./p'
alias setdefaults='dconf reset -f /'

# Ignore these commands
#
export PAGER='/usr/bin/less -ins'
export COLUMNS=108

# SCALE
#
export TEXT_SCALE=1.5

alias get_scale='gsettings get org.gnome.desktop.interface text-scaling-factor'
alias_set_scale='gsettings set org.gnome.desktop.interface text-scaling-factor'

#if [ -z "$IP" ]; then
#  if [ !-z "$IFC" ]; then
#    export IP=$(ifconfig | grep -A1 BROADCAST,RUNNING,MULTICAST | grep inet | cut -d' ' -f10)
#  fi
#fi

export SHOW_CPP_INCLUDES='g++ -E -Wp,-v -xc /dev/null'
export SHOW_LD_PATHS="ld --verbose | grep SEARCH_DIR | tr -s ' ;' \\012"

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

if [ -z "$THIS_ARCH" ]; then
    THIS_ARCH=$(uname -m)
fi
if [[ "$THIS_ARCH" = "x86_64" ]]; then
    PROMPT_COLOR=${BOLD}${GREEN}
else
    PROMPT_COLOR=${BOLD}${YELLOW}
fi
export PS1=${PROMPT_COLOR}'\h[${THIS_ARCH} ${OS_DISTRO} ${OS_RELEASE}] \W> '${NORMAL}
