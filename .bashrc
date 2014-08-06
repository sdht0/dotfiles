# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
        . /etc/bashrc
fi

# User specific aliases and functions
bind '"\e[A": history-search-backward'
bind '"\e[B": history-search-forward'

export PS1='`if [ $? = 0 ]; then echo "\[\033[01;32m\]✔"; else echo "\[\033[01;31m\]✘"; fi` \[\033[01;30m\][\u@\h]\[\033[01;34m\] \W\[\033[35m\]$(__git_ps1 " (%s)") \[\033[01;30m\]$\[\033[00m\] '

alias ls='ls -h --color=auto'
alias ll='ls -lh --color=auto'
alias grep="grep --color=auto"

alias yumu='sudo yum update'
alias yumi='sudo yum install'
alias yums='sudo yum search'

alias pcmu='sudo pacman -Syu'
alias pcmi='sudo pacman -S'
alias pcms='sudo pacman -Ss'
alias pcmsl='sudo pacman -Qs'
alias pcmr='sudo pacman -Rc'
alias pcmc='sudo pacman -Sc --noconfirm'

sstart() { sudo systemctl start $1.service ; sudo systemctl status -l $1.service; }
srestart() { sudo systemctl restart $1.service ; sudo systemctl status -l $1.service; }
sstop() { sudo systemctl stop $1.service ; sudo systemctl status -l $1.service; }
sstatus() { sudo systemctl status -l $1.service; }
sreload() { sudo systemctl reload $1.service; }
senable() { sudo systemctl enable $1.service ; ls -l /etc/systemd/system/multi-user.target.wants; }
sdisable() { sudo systemctl disable $1.service ; ls -l /etc/systemd/system/multi-user.target.wants; }

#sstart() { sudo service $1 start ; }
#srestart() { sudo service $1 restart ; }
#sstop() { sudo service $1 stop ; }
#sstatus() { sudo service $1 status ; }
#sreload() { sudo service $1 reload ; }

mkcd() {
    mkdir -p $1 && cd $1
}

xs() {
    if [ $# -lt 1 ]; then
        echo "No input!"
        return 1
    fi

    grep --color=auto -Rn $* *
}

xgitc() {
    if [ "$1" = "" ]; then
        mstatus="updates: $(date +'%Y-%m-%d %H:%M:%S')"
        printf "You've not set any message: '$mstatus' is being used [Y|n]: "
        read inp
        if [ "$inp" = "n" ]; then
            return 1
        fi
    else
        mstatus="$1"
    fi
    git add -A .
    git commit -a -m "$mstatus"
    if [ "$2" != "" ]; then
        git push -u origin master
    fi
}

