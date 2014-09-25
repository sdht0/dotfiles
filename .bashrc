
# Replace with a tmux session if it is an interactive session and tmux is installed and is not already running
if [[ $- =~ i ]] && which tmux > /dev/null 2>&1 && [[ -z "$TMUX" ]] ;then
    ID="`tmux ls | grep -vm1 attached | cut -d: -f1`" # get the id of a deattached session
    if [[ -z "$ID" ]] ;then # if not available create a new one
        exec tmux new-session
    else
        exec tmux attach-session -t "$ID" # if available attach to it
    fi
fi

export LANG='en_US.UTF-8'
export EDITOR='vim'

MYSHELL=$(ps -p $$ -ocomm= 2>/dev/null)

if [[ "$MYSHELL" = 'bash' ]];then
    bind '"\e[A": history-search-backward'
    bind '"\e[B": history-search-forward'

    shopt -s nocaseglob;                    # Case-insensitive globbing (used in pathname expansion)
    shopt -s histappend;                    # Append to the Bash history file, rather than overwriting it
    shopt -s cdspell;                       # Autocorrect typos in path names when using `cd`
fi

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

[[ $UID -eq 0 ]] && color=${BRed} || color=${BGreen}
[[ $UID -eq 0 ]] && prompt='#' || prompt='$'
export PS1="\n[${BBlue}${MYSHELL}${Color_Off}:${color}\u@\h${Color_Off}] ${BPurple}\w\n`if [ $? = 0 ]; then echo "${BGreen}✔"; else echo "${BRed}✘"; fi`${Color_Off} ${prompt} "

alias ls='ls -h --color=auto'
alias ll='ls -lh --color=auto'
alias rr='rm -rf'
alias mount='mount -v'
alias umount='umount -v'
alias rmdir='rmdir -v'
alias pu='pushd'
alias po='popd'
alias dmesg='dmesg --human -T'
alias jlog='sudo journalctl -n500 -f'
alias gitk="gitk --all"
alias grep="grep --color=auto"
alias vi='vim'

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

alias ccm='sudo ccm64'
alias xcdwebfol='cd /home/lfiles/www'
alias xcddev='cd /home/lfiles/dev'
alias xcdactive='cd /home/lfiles/dev/sources/telepathy-kde-active/ktp-active/application/package'
alias xchromestart="chromium --proxy-server='socks://127.0.0.1:9999' --incognito"
alias xstartproxy="ssh -TNfD 9999 root@5.175.167.132"
alias xstartproxy2="ssh -TNfD '*:9999' -p 9999 dcadmin@172.16.32.222"

# Systemd service management
sstart() { sudo systemctl start $1.service ; sudo systemctl status -l $1.service; }
srestart() { sudo systemctl restart $1.service ; sudo systemctl status -l $1.service; }
sstop() { sudo systemctl stop $1.service ; sudo systemctl status -l $1.service; }
sstatus() { sudo systemctl status -l $1.service; }
sreload() { sudo systemctl reload $1.service; }
senable() { sudo systemctl enable $1.service ; ls -l /etc/systemd/system/multi-user.target.wants; }
sdisable() { sudo systemctl disable $1.service ; ls -l /etc/systemd/system/multi-user.target.wants; }

# Init scripts service management
ustart() { sudo service $1 start ; }
urestart() { sudo service $1 restart ; }
ustop() { sudo service $1 stop ; }
ustatus() { sudo service $1 status ; }
ureload() { sudo service $1 reload ; }

h() { if [ -z "$*" ]; then history 1; else history 1 | egrep "$@"; fi; }

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

xgipull() {
    case "$1" in
        xvcs) 1='/xfiles/source-code-repository/'
            ;;
        *) 1=$(pwd)
            ;;
    esac

    walkfolderandpull() {
        pushd "$1" > /dev/null || return -1
        if [ -d ".git" ];then
            printf "\n********Updating git repo $1********\n"
            git pull --all
        elif [ -d ".bzr" ];then
            printf "\n********Updating bzr repo $1********\n"
            bzr update
        elif [ -d ".svn" ];then
            printf "\n********Updating svn repo $1********\n"
            svn fetch
        else
            printf "\n********Entering $1********\n"
            for i in *;do
            if [ -d "$i" ];then
                walkfolderandpull "$i"
            fi
            done
        fi
        popd > /dev/null || return -1
    }

    walkfolderandpull "$1"
}

xuploadpicasa() {
    if [ $# -lt 1 ];then
        echo "Usage: xuploadpicasa albumname [nocreate]"
        return
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

xuploadskydrive() {
    if [ $# -lt 2 ];then
        echo "Usage: xuploadskydrive local_file|local_folder remotefolder"
        return
    fi

    pd=$(pwd)

    control_c() {
        cd "$pd"
        return
    }

    walkfolderandupload() {
        trap control_c SIGINT

        if [ -d "$1" ];then
            printf "Entering directory '$1' using '$2'\n"
            pushd "$1" > /dev/null || return 1
            printf "Creating remote folder '$2/$(basename "$1")'\n"
            onedrive-cli $3 $4 $5 mkdir "$2/$(basename "$1")" &> /dev/null
            for i in *;do
                walkfolderandupload "$i" "$2/$(basename "$1")"  $3 $4 $5 || return 3
            done
            printf "Exiting directory '$1' using '$2'\n"
            popd > /dev/null || return 2
        elif [ -f "$1" ];then
            printf "Uploading file '$1' to '$2/'\n"
            onedrive-cli $3 $4 $5 put -n "$1" "$2" || return 0
        else
            echo "Unrecognized object '$1' in '$(pwd)'"
        fi
    }

    trap control_c SIGINT

    walkfolderandupload "$1" "$2" $3 $4 $5
    control_c
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

xlistfiles() {
    case "$1" in
        hdd1) 1='/home/lfiles/sdh-hdd'
            flname=files-hdd1
            ;;
        hdd2) 1='/home/lfiles/sdh-hdd2'
            flname=files-hdd2
            ;;
        *) 1='/xfiles'
            flname=files
            ;;
    esac

    find "$1" -type d \( -name ".git" -o -name ".hg" -o -name "\$RECYCLE.BIN" -o -name "System Volume Information" -o -name "version-controlled-soft" -o -name "manuals" -o -name "eclipse" \) -prune -o -print | sort > /xfiles/my-downloads/${flname}.txt
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
