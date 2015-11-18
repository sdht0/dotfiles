
# Replace with a tmux session if it is an interactive session and tmux is installed and is not already running
if [[ $UID -ne 0 ]] && [[ $- = *i* ]] && which tmux > /dev/null 2>&1 && [[ -z "$TMUX" ]] && [[ ! $TTY = *tty* ]] ;then
    ID="`tmux ls | grep -vm1 attached | cut -d: -f1`" # get the id of a deattached session
    if [[ -z "$ID" ]] ;then # if not available create a new one
        exec tmux new-session 2>/dev/null
    else
        exec tmux attach-session -t "$ID" 2>/dev/null # if available attach to it
    fi
fi

export LANG='en_US.UTF-8'
export EDITOR='vim'
export PATH="/sbin:/usr/sbin::$PATH:/usr/lib64/nagios/plugins:/usr/lib64/nagios/plugins/custom"
export HISTFILESIZE=100000
export HISTSIZE=${HISTFILESIZE}
export HISTFILE=~/.bash_history
PROMPT_COMMAND="history -a; $PROMPT_COMMAND"

MYSHELL=$(ps -p $$ -ocomm= 2>/dev/null)

if [[ $- = *i* ]] && [[ "$MYSHELL" = 'bash' ]];then

    bind '"\e[A": history-search-backward'
    bind '"\e[B": history-search-forward'

    shopt -s nocaseglob 2>/dev/null                    # Case-insensitive globbing (used in pathname expansion)
    shopt -s histappend 2>/dev/null                    # Append to the Bash history file, rather than overwriting it
    shopt -s cdspell 2>/dev/null                       # Autocorrect typos in path names when using `cd`
    shopt -s autocd 2>/dev/null
    shopt -s checkwinsize 2>/dev/null
    shopt -s cmdhist 2>/dev/null                       # force multi-line commands to be stored in the history as a single line
    shopt -s no_empty_cmd_completion 2>/dev/null

    # Reset
    Color_Off="\[\033[0m\]"       # Text Reset

    # Regular Colors
    Black="\[\033[0;30m\]"        # Black
    Red="\[\033[0;31m\]"          # Red
    Green="\[\033[0;32m\]"        # Green
    Yellow="\[\033[0;33m\]"       # Yellow
    Blue="\[\033[0;34m\]"         # Blue
    Purple="\[\033[0;35m\]"       # Purple
    Cyan="\[\033[0;36m\]"         # Cyan
    White="\[\033[0;37m\]"        # White

    # Bold
    BBlack="\[\033[1;30m\]"       # Black
    BRed="\[\033[1;31m\]"         # Red
    BGreen="\[\033[1;32m\]"       # Green
    BYellow="\[\033[1;33m\]"      # Yellow
    BBlue="\[\033[1;34m\]"        # Blue
    BPurple="\[\033[1;35m\]"      # Purple
    BCyan="\[\033[1;36m\]"        # Cyan
    BWhite="\[\033[1;37m\]"       # White

    [[ $UID -eq 0 ]] && color=${BRed} || color=${BPurple}
    [[ $UID -eq 0 ]] && prompt='#' || prompt='$'
    export PS1="\n[${BBlue}${MYSHELL}${Color_Off}:${color}\u@\H${Color_Off}] ${BGreen}\w\n\$([[ \$? == 0 ]] && echo \"${BGreen}\" || echo \"${BRed}\")${prompt}${Color_Off} "

fi

alias ..='cd ..'
alias ...='cd ../..'
alias b='cd -'
alias ls='sudo ls --color=auto'
alias lsa='sudo ls -CFah --color=auto'
alias ll='sudo ls -CFlh --color=auto'           # Use ll -t, ll -tr, ll -S, ll -Sr for more sorting options. Similarly for lla and lld
alias lla='sudo ls -CFalh --color=auto'
lld() { sudo ls -CFalh $* --color=force | grep -e "^d" -e total --color=never;sudo ls -CFalh $* --color=force | grep -vE "^d|total"; }
alias lldt='sudo ls -CFalht --color=force | grep -e "^d" -e total --color=never;sudo ls -CFalht --color=force | grep -vE "^d|total"'
alias llds='sudo ls -CFalhS --color=force | grep -e "^d" -e total --color=never;sudo ls -CFalhS --color=force | grep -vE "^d|total"'
cl() { cd "$@" && lls; }
alias lsg='sudo ls -CFalh --color=auto | grep --color=auto -i'
alias psg='sudo ps aux | grep -v grep | grep -i -e VSZ -e '
alias psgc='sudo ps aux | grep -v grep | grep -i -e '
alias pkl='sudo kill -9'
alias lsofs='sudo lsof | grep'
alias portso='sudo netstat -tulnp | grep LISTEN | sort -k6'
alias portsa='sudo netstat -tulanp'
ports() {
    [[ -z "$1" ]] && 1="b"
    [[ -z "$2" ]] && 2="5"
    if [[ "$1" == "b" ]];then
        ports u "$2"
        echo
        ports t "$2"
        return 0
    fi
    echo -e "Proto Recv-Q Send-Q LocalAddress Port ForeignAddress PID ProgramName\n$(sudo netstat -lnp${1} | tail -n +3 | sed -r -e "s/LISTEN(.*)/\1 LISTEN/" -e "s|:([0-9]+) | \1 |" -e "s|/| |" | sort -n -k${2})" | column -t
}
alias mkdir="mkdir -p"
alias smkdir="sudo mkdir -p"
alias rr='sudo rm -rf'
alias mount='sudo mount -v'
alias umount='sudo umount -v'
alias pu='pushd'
alias cat='sudo cat'
alias po='popd'
alias dmesg='sudo dmesg --human -T'
alias gitk='gitk --all'
alias grep='sudo grep -i --color=auto'
alias crns='sudo crontab -l'
alias crne='sudo crontab -e'
alias crnsu='sudo crontab -l -u'
alias tail='sudo tail'
alias tn='sudo tail -n'
alias tf='sudo tail -f'
alias vi='vim'
alias se='sudoedit'
alias sv='sudo vim -u ~/.vimrc'
alias e='vim'
alias tarc="tar czf"
alias tarx="tar xzf"
alias starc="sudo tar czf"
alias starx="sudo tar xzf"
alias myips='ip -o -f inet addr | grep -v "127.0.0.1" | cut -d'/' -f1 | sed -r "s/[ \t]+/ /g" | cut -d" " -f2-4 | awk "{print \$1\": \"\$3}" | sort | uniq'
alias dateh='date --help|sed -n "/^ *%%/,/^ *%Z/p"|while read l;do F=${l/% */}; date +%$F:"|'"'"'${F//%n/ }'"'"'|${l#* }";done|sed "s/\ *|\ */|/g" |column -s "|" -t'
alias jlog='sudo journalctl -n500 -f'
alias xcp='xclip -selection clipboard'
alias httpserver="python2 -m SimpleHTTPServer"
alias sx="startx"
alias sd="sudo shutdown now"
alias sa="ssh-add"

alias please='sudo $(fc -ln -1)'
alias pleaseplease='sudo $(history | tail -1 | awk "{\$1=\"\";print}" | xargs)'

alias rzsh='. ~/.bashrc && . ~/.zshrc'
alias rbash='. ~/.bashrc'

alias ga='git add'
alias gc='git commit -m'
alias gca='git commit -am'
alias gs='git status'
alias gt='git stash'
alias gtp='git stash pop'
alias gl='git ls'
alias gcl='git clone'
alias gb='git branch -a'
alias gpu='git push'
alias gp='git pull'
alias gpgp='git pull --rebase && git push'
alias gcf='git config --list'

alias dk='sudo docker'
alias dkr='sudo docker run'
alias dki='sudo docker images'
alias dkia='sudo docker images -a'
alias dkc='sudo docker ps'
alias dkca='sudo docker ps -a'
dkrc() { sudo docker start $1 && sudo docker attach $1;}
dkrm() { sudo docker kill $@; sudo docker rm $@; }

# Pacman package management
alias pcmu='sudo pacman -Syu'
alias pcmi='sudo pacman -S'
alias pcms='sudo pacman -Ss'
alias pcmsl='sudo pacman -Qs'
alias pcmr='sudo pacman -Rc'
alias pcmc='sudo pacman -Sc --noconfirm'

# Apt-get package management
alias agu='sudo apt-get update && sudo apt-get upgrade'
alias agi='sudo apt-get install'
alias ags='sudo apt-cache search'
alias agr='sudo apt-get remove'

# Yum package management
alias yumu='sudo yum update'
alias yumi='sudo yum install'
alias yums='sudo yum search'
alias yumr='sudo yum remove'
alias yuml='sudo yum --showduplicates list'
alias yumf='sudo yum --showduplicates info'

alias jetpistol='sudo puppet agent -t --configtimeout=900'
alias gomugomuno='echo "Waiting 5s..." && sleep 5; echo "Running puppet.." && jetpistol'
alias osv='cat /etc/*-release | sort | uniq | xargs -L1'
alias tfp="sudo tail -f /var/log/puppet/puppet.log"
alias tfa="sudo tail -f /var/log/httpd/access_log"
alias tfe="sudo tail -f /var/log/httpd/error_log"
alias tnp="sudo tail /var/log/puppet/puppet.log -n"
alias tna="sudo tail /var/log/httpd/access_log -n"
alias tne="sudo tail /var/log/httpd/error_log -n"
alias magicm2='cd;sudo openvpn --config ~/directi/client.ovpn'
alias magicm='cd;sudo openvpn --config ~/directi/mnet-client.ovpn'
alias magic2='cd;~/dotfiles/scripts/startOpenVPN.sh ~/directi/client.ovpn `~/sshhhh mnetu | base64 --decode` `~/sshhhh mnetp | base64 --decode` `~/sshhhh mnetc | base64 --decode | python2 ~/dotfiles/scripts/gauthenticator.py`'
alias magic='cd;~/dotfiles/scripts/startOpenVPN.sh ~/directi/mnet-client.ovpn `~/sshhhh mnetu | base64 --decode` `~/sshhhh mnetp | base64 --decode` `~/sshhhh mnetc2 | base64 --decode | python2 ~/dotfiles/scripts/gauthenticator.py`'

s() {
    [[ $# -lt 1 ]] && echo "No input!" && return 1
    case "$1" in
        pr*) ssh c8-proxy-"${1:2}".srv.media.net ${*:2};;
        lg*) ssh c8-logging-"${1:2}".srv.media.net ${*:2};;
        ds*) ssh c8-data-store-"${1:2}".srv.media.net ${*:2};;
        lr*) ssh c8-logging-redis-"${1:2}".srv.media.net ${*:2};;
        lk*) ssh c8-logging-kafka-"${1:2}".srv.media.net ${*:2};;
        w8*) ssh c8-web-"${1:2}".srv.media.net ${*:2};;
        w10*) ssh c10-web-"${1:3}".srv.media.net ${*:2};;
        w12b*) ssh c12-nc1b-web-"${1:4}".srv.media.net ${*:2};;
        w12c*) ssh c12-nc1c-web-"${1:4}".srv.media.net ${*:2};;
        ddrc) ssh c12-nc1c-dadar.srv.media.net ${*:2};;
        ddrb) ssh c12-nc1b-dadar.srv.media.net ${*:2};;
        *) ssh ${*};;
    esac
}

alias ccm='sudo ccm64'
alias xcdwebfol='cd /srv/www'
alias xcddev='cd /home/sdh/dev'
alias xcdactive='cd /home/sdh/dev/sources/telepathy-kde-active/ktp-active/application/package'
alias xchromestart="chromium --proxy-server='socks://127.0.0.1:9999' --incognito"
alias xstartproxy="ssh -TNfD 9999 root@5.175.167.132"
alias xstartproxy2="ssh -TNfD '*:9999' -p 9999 dcadmin@172.16.32.222"

# Systemd service management
sds() { sudo systemctl status -l $1.service; }
sdst() { sudo systemctl start $1.service ; sudo systemctl status -l $1.service; }
sdsp() { sudo systemctl stop $1.service ; sudo systemctl status -l $1.service; }
sdr() { sudo systemctl restart $1.service ; sudo systemctl status -l $1.service; }
sdrl() { sudo systemctl reload $1.service; }
sde() { sudo systemctl enable $1.service ; ls -l /etc/systemd/system/multi-user.target.wants; }
sdd() { sudo systemctl disable $1.service ; ls -l /etc/systemd/system/multi-user.target.wants; }

# Init scripts service management
ups() { sudo service $1 status ; }
upst() { sudo service $1 start ; }
upsp() { sudo service $1 stop ; }
upr() { sudo service $1 restart ; }
uprl() { sudo service $1 reload ; }
upe() { sudo chkconfig --add $1 && sudo chkconfig $1 on && sudo chkconfig --list $1 ; }
upd() { sudo chkconfig $1 off && sudo chkconfig --list $1 ; }

mypublicip() {
    printf "dig +short @resolver1.opendns.com myip.opendns.com\ndig +short -t txt @ns1.google.com o-o.myaddr.l.google.com\ncurl -s ident.me\ncurl -s icanhazip.com" | xargs -L1 -P0 -I{} sh -c 'x=$({} | tr -d "\"";echo " | {}");echo $x'
}

mkcd() {
    mkdir -p $1 && cd $1
}

ctg() {
    cat "$1" | grep "$2"
}

xs() {
    if [ $# -lt 1 ]; then
        echo "No input!"
        return 1
    fi

    sudo grep --color=auto -Rn $* *
}

xf() {
    if [ $# -lt 1 ]; then
        echo "No input!"
        return 1
    fi

    sudo find -name "*$**"
}

h() { if [ -z "$*" ]; then history 1; else history 1 | grep -E "$@"; fi; }

rand() { < /dev/urandom tr -dc "A-Za-z0-9${2:-@#$%^&*}" | head -c${1:-16};echo;}

up() {
    if [ -z "$*" ]; then 1='1';fi
    pd=`pwd`
    cd $(eval printf '../'%.0s {1..$1}) && echo "${pd} -> $(pwd)";
}

fawk() {
    first="awk '{print "
    last="}'"
    cmd="${first}\$${1}${last}"
    eval $cmd
}

pkla() {
    if [ $# -lt 1 ]; then
        echo "No input!"
        return 1
    fi
    sudo ps aux | grep -v grep | grep -i -e $1 | awk '{print $2}' | xargs sudo kill -9
}

gitkf() {
  gitk_follow () {
    while (( "$#" )); do
      git log -p --oneline --name-status --follow $1;
      shift;
    done | perl -ne 'if( s{^(?:[ACDMRTUXB]|R\d+)\s+}{} ) { s{\s+}{\n}g; print; }' | sort -u
  }
  gitk $(gitk_follow $*)
}

man() {
    env LESS_TERMCAP_mb=$'\E[01;31m' \
    LESS_TERMCAP_md=$'\E[01;38;5;74m' \
    LESS_TERMCAP_me=$'\E[0m' \
    LESS_TERMCAP_se=$'\E[0m' \
    LESS_TERMCAP_so=$'\E[38;5;246m' \
    LESS_TERMCAP_ue=$'\E[0m' \
    LESS_TERMCAP_us=$'\E[04;38;5;146m' \
    man "$@"
}

changehostname() {
    if [ $# -lt 2 ]; then
        echo "Usage: changehostname centos|ubuntu hostname"
        return 1
    fi
    os="$1"
    nhostname="$2"

    case "$os" in
    centos)
        [[ -r '/etc/sysconfig/network' ]] && sudo sed -i "/HOSTNAME/ s/^.*$/HOSTNAME=${nhostname}/" /etc/sysconfig/network || { echo "File not found"; return -1; }
        ;;
    ubuntu)
        [[ -r '/etc/hostname' ]] && echo $nhostname | sudo tee /etc/hostname > /dev/null
        ;;
    *) echo "No match!"
        ;;
    esac
    sudo hostname "$nhostname"
    [[ -r '/etc/hosts' ]] && grep -v "$nhostname" /etc/hosts > /dev/null || sudo sed -i "/127.0.0.1/ s/$/ $nhostname/" /etc/hosts
}

xdeletefromgit() {
    if [ $# -eq 0 ]; then
        return 0
    fi

    # make sure we're at the root of git repo
    if [ ! -d .git ]; then
        echo "Error: must run this script from the root of a git repository"
        exit 1
    fi

    # remove all paths passed as arguments from the history of the repo
    files=$@
    git filter-branch --index-filter "git rm -rf --cached --ignore-unmatch $files" HEAD

    # remove the temporary history git-filter-branch otherwise leaves behind for a long time
    rm -rf .git/refs/original/ && git reflog expire --all &&  git gc --aggressive --prune
}

xreauthorgit() {
    if [ $# -lt 3 ]; then
        return 0
    fi

git filter-branch --env-filter '

OLD_EMAIL="'"$1"'"
CORRECT_NAME="'"$2"'"
CORRECT_EMAIL="'"$3"'"

if [ "$GIT_COMMITTER_EMAIL" = "$OLD_EMAIL" ]
then
export GIT_COMMITTER_NAME="$CORRECT_NAME"
export GIT_COMMITTER_EMAIL="$CORRECT_EMAIL"
fi
if [ "$GIT_AUTHOR_EMAIL" = "$OLD_EMAIL" ]
then
export GIT_AUTHOR_NAME="$CORRECT_NAME"
export GIT_AUTHOR_EMAIL="$CORRECT_EMAIL"
fi
' --tag-name-filter cat -- --branches --tags
}

xextract() {
    if [[ -f "$1" ]]; then
        case "$1" in
            *.lrz) lrztar -d "$1" ;;
            *.tar.bz2) tar xjf "$1";;
            *.tar.gz) tar xzf "$1" ;;
            *.tar.xz) tar Jxf "$1" ;;
            *.bz2) bunzip2 "$1";;
            *.rar) rar x "$1";;
            *.gz) gunzip "$1"  ;;
            *.tar) tar xf "$1"  ;;
            *.tbz2) tar xjf "$1" ;;
            *.tgz) tar xzf "$1" ;;
            *.zip) unzip "$1" ;;
            *.Z) uncompress "$1"  ;;
            *.7z) 7z x "$1" ;;
            *) echo "don't know how to extract '$1'..." ;;
        esac
    else
    echo "'$1' is not a valid file!"
    fi
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

xgiaction() {
    if [ $# -lt 1 ];then
        echo "Usage: xgiaction action [arguments]"
        return
    fi

    walkfolderandexecute() {
        pushd "$1" > /dev/null || return -1
        shift
        if [ -d ".git" ];then
            printf "\n********git $1 in $(pwd)********\n"
            git $*
        elif [ -d ".bzr" ];then
            if [ "$1" = "pull" ];then
                printf "\n********Updating bzr repo $(pwd)********\n"
                bzr update
            fi
        elif [ -d ".svn" ];then
            if [ "$1" = "pull" ];then
                printf "\n********Updating svn repo $(pwd)********\n"
                svn fetch
            fi
        else
            printf "\n********Entering $(pwd)********\n"
            for i in *;do
            if [ -d "$i" ];then
                walkfolderandexecute "$i" $*
            fi
            done
        fi
        popd > /dev/null || return -1
    }

    walkfolderandexecute "." $*
}

xuploadpicasa() {
    if [ $# -lt 1 ];then
        echo "Usage: xuploadpicasa albumname [nocreate]"
        return 1
    fi
    f="resized"
    n=$1
    mkdir -p "$f"
    for i in *;do
        echo $i
        [[ -f "$i" ]] && convert "$i" -resize "2048x2048>" "$f/$i"
    done
    cd "$f" && \
    [[ "$2" != "nocreate" ]] && google -v --access=private picasa create "$n" || echo "Reusing $1" && \
    google -v picasa post "$n" * && \
    cd ..
}

xmakecustomarchiso() {
    if [ $# -lt 1 ]; then
        echo "Need iso path and iso label!"
        return 1
    fi
    isoname=sdh-$(basename "$1")
    isolabel=$(isoinfo -d -i "$1" | grep "Volume id" | sed "s/Volume id: //")
    outputdir=$(pwd)
    sudo umount /tmp/sdh-customiso/mnt > /dev/null 2>&1
    rm -rf /tmp/sdh-customiso
    echo "Creating dirs..." && \
    mkdir -p /tmp/sdh-customiso/{,mnt,contents} && \
    cd /tmp/sdh-customiso && \
    echo "Mounting iso..." && \
    sudo mount -o loop "$1" /tmp/sdh-customiso/mnt/ && \
    echo "Extracting contents..." && \
    cp -a mnt/* contents && \
    cd contents && \
    echo "Copying scripts..." && \
    rsync --copy-links -vr /home/lfiles/dev/archlinux-install-scripts . && \
    chmod +x archlinux-install-scripts/* && \
    echo "Creating new iso '$isoname' to $outputdir..." && \
    genisoimage -r -V "$isolabel" -cache-inodes -J -l -b isolinux/isolinux.bin -c isolinux/boot.cat -no-emul-boot -boot-load-size 4 -boot-info-table -o /tmp/sdh-customiso/"$isoname" . && \
    mv /tmp/sdh-customiso/"$isoname" $outputdir
    echo "Cleaning up..."
    sudo umount /tmp/sdh-customiso/mnt
    #rm -rf /tmp/sdh-customiso
    cd $outputdir
    echo "Done."
}

xget() {
    if [ $# -lt 1 ]; then
        echo "No input! [$(cat sshhhh | grep ')' | grep -v '*' | grep -v 'mnet' | cut -f1 -d')' | xargs | tr ' ' '|')]"
        return 1
    fi
    code=$(~/sshhhh "$1" | base64 --decode | python2 ~/dotfiles/scripts/gauthenticator.py)
    printf $code | xclip -selection clipboard
    echo "Copied $code to clipboard"
}

xlistfiles() {
    case "$1" in
        hdd1) 1='/run/media/sdh/sdh-hdd1'
            flname=files-hdd1
            ;;
        hdd2) 1='/run/media/sdh/sdh-hdd2'
            flname=files-hdd2
            ;;
        hdd3) 1='/run/media/sdh/sdh-hdd3'
            flname=files-hdd2
            ;;
        hdd4) 1='/run/media/sdh/sdh-hdd4'
            flname=files-hdd2
            ;;
        *)  echo "Wrong input"
            return -1
            ;;
    esac

    find "$1" -type d \( -name ".git" -o -name ".hg" -o -name "Dev" -o -name "\$RECYCLE.BIN" -o -name "System Volume Information" -o -name "version-controlled-soft" -o -name "manuals" -o -name "eclipse" \) -prune -o -print | sort > ~/Downloads/${flname}.txt
}

xxown() {
    sudo chown -R $(whoami):http .
}

xintegration() {
    git stash && \
    git checkout master && \
    git pull && \
    for i in $(git branch | grep sdh);do
        git checkout "$i";
        git rebase master;
    done && \
    git checkout master && \
    git branch -D integration || true && \
    git checkout -b integration && \
    for i in $(git branch | grep sdh);do
        git merge "$i";
    done
}

xplaylist() {
    if [ $# -lt 1 ]; then
    echo "No input!"
    return 1
    fi

    cd $1

    if [ "$2" == "" ];then
    ext="mp4"
    else
    ext=$2
    fi

    for i in *; do
        if [ -d "$i" ];then
            echo $i
            printf "#EXTM3U\n" > $i.m3u
            for j in $(for k in $(ls $i/*\.$ext); do echo $k;done | sort -V); do
                printf "#EXTINF:-1,$j\n$j\n" >> $i.m3u
            done
        fi
    done
}

xkdechanges() {
    dayz=${1:-7}
	for i in *;do 
		[[ -d "$i/.git" ]] && echo "$i" && { [[ -n "$(git --git-dir="$i/.git" --work-tree="$i" log --since="$dayz day ago" --pretty=oneline)" ]] && cd $i && echo "    $(git log --since="$dayz day ago" --pretty=oneline | wc -l) commits" && { gitk --since="$dayz day ago" || true; } && cd .. || echo "    No new commits"; }
	done
}

xpatternrename() {
    if [ $# -lt 2 ]; then
        echo "Please give two patterns!"
        return 1
    fi
    for i in *;do x=$(echo $i | sed "s|$1|$2|"); if [ ! -r "$x" ];then echo "$i->$x"; mv "$i" "$x";fi;done
}

xgpp() {
    if [ $# -lt 1 ]; then
    echo "No input files!"
    return 1

    elif [ $# -eq 2 ]; then
    g++ $1 && ./a.out < $2

    else
    g++ $1 && ./a.out
    fi

    rm -f a.out
}

xgcc() {
    if [ $# -lt 1 ]; then
    echo "No input files!"
    return 1

    elif [ $# -eq 2 ]; then
    gcc $1 && ./a.out < $2 && rm a.out

    else
    gcc $1 && ./a.out  && rm a.out
    fi
}

xccreatefolder() {
    if [ $# -ne 1 ]; then
    echo "Wrong input!"
    return 1
    fi

    mkdir -p $1 && ( echo '#include<stdio.h>
int main() {

    return 0;
}' > $1/$1.c ; touch $1/in$1.txt )
}

xcppcreatefolder() {
    if [ $# -ne 1 ]; then
    echo "Wrong input!"
    return 1
    fi

    mkdir -p $1 && ( echo '#include<iostream>
using namespace std;
int main() {

    return 0;
}' > $1/$1.cpp ; touch $1/in$1.txt )
}

xregexrename() {
  if [ $# -lt 1 ]; then
    echo "Wrong input!"
    return 1
  fi
  for i in *;do
    x=$(echo $i | sed "$1"); [[ ! -r "$x" ]] && echo "$i -> $x" && [[ "$2" = "m" ]] && mv "$i" "$x"
  done
}

xrenametotitlecase() {
    pd=$(pwd)
    if [ ! -z "$1" ];then
        pd="$1"
    fi
    pushd "$pd" > /dev/null && \
    for i in *;do x=$(echo $i | tr "[A-Z]" "[a-z]" | sed "s/\( \| \?-\| \?(\| \?\[\|^\)\(.\)/\1\u\2/g");[ ! -r "$x" ] && mv "$i" "$x";done && \
    popd > /dev/null
}

xrenametolowercase() {
    depth=0
    maxdepth=1
    rename() {
        if [ ! -z "$1" ];then
            echo "Inside directory: $1"
            pushd $1 > /dev/null
            for i in *;do
                    if [ -d "$i" ];then
                        if [ "$i" = "System Volume Information" -o "$i" = "\$RECYCLE.BIN" ];then
                            echo "Skipping $i"
                            continue
                        fi
                        dr=$i
                        x=$(echo $i|tr '[A-Z]' '[a-z]'|tr ' ' '-')
                        if [ ! -d "$x" ];then
                            echo "Dir - $i: ### Renaming to $x ###"
                            dr=$x
                            mv "$i" "$x"
                        elif [ "$x" != "$i" ];then
                            echo "Dir - $i: *** Not renamed: dir $x already exists ***"
                        else
                            echo "Dir - $i: OK"
                        fi
                        if test $depth -lt $maxdepth;then
                            echo $((depth++))
                            rename "$1/$dr"
                            echo $((depth--))
                        fi
                    fi
            done
            for i in *;do
                if [ -f "$i" ];then
                    x=$(echo $i|tr '[A-Z]' '[a-z]'|tr ' ' '-')
                    if [ ! -f "$x" ];then
                        echo "File - $i: ### Renaming to $x ###"
                        mv "$i" "$x"
                    elif [ "$x" != "$i" ];then
                        echo "File - $i: *** Not renamed: file $x already exists ***"
                    else
                        echo "File - $i: OK"
                    fi
                fi
            done
            echo "Leaving directory: $1"
            popd > /dev/null
        fi
    }
    pd=$(pwd)
    if [ ! -z "$1" ];then
    pd=$1
    fi
    if [ ! -z "$2" ] && [ "$2" = "-d" ]
        then
        if [ ! -z "$3" ];then
            maxdepth=$3
        fi
    fi
    rename "$pd"
}
