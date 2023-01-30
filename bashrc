
# Replace with a tmux session if it is an interactive session and tmux is installed and is not already running
if [[ $UID -ne 0 ]] && [[ $- = *i* ]] && which tmux > /dev/null 2>&1 && [[ -z "$TMUX" ]] && [[ -z "$NOTMUX" ]] && [[ ! $TTY = *tty* ]] ;then
    ID="`tmux ls | grep -vm1 attached | cut -d: -f1`" # get the id of a deattached session
    if [[ -z "$ID" ]] ;then # if not available create a new one
        exec tmux new-session 2>/dev/null
    else
        exec tmux attach-session -t "$ID" 2>/dev/null # if available attach to it
    fi
fi

export EDITOR='vim'
export VISUAL=$EDITOR
export HISTFILESIZE=100000
export HISTSIZE=${HISTFILESIZE}
export HISTFILE=~/.bash_history
PROMPT_COMMAND="history -a; $PROMPT_COMMAND"

MYSHELL=$(ps -p $$ -ocomm= 2>/dev/null)

[[ $- = *i* ]] && stty -ixon
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

    [[ $UID -eq 0 ]] && prompt='#' || prompt='$'
    export PS1="\n[${BBlue}${MYSHELL}${Color_Off}][\t] ${BYellow}\w\n\$([[ \$? == 0 ]] && echo \"${BGreen}\" || echo \"${BRed}\")${prompt}${Color_Off} "

fi

_checkargs() {
	[[ $# -lt 2 ]] && echo "Incorrect check usage" && return 2
    if [[ $1 -lt $2 ]]; then
        echo "Atleast $2 argument(s) expected!"
        return 1
    fi
	return 0
}

# Enable `sudo alias`
alias sudo='sudo '

# Enable `sudo bash_function`
sudof() {
    if [[ $# -lt 1 || -z "$1" ]]; then
        echo "Error: Missing input arguments!"
        return 1
    fi
    fn_name=$1
    shift
    FUNC=$(declare -f $fn_name)
    [[ -z "$FUNC" ]] && { echo "Error: Function '$fn_name' not found"; return; }
    sudo bash -c "$FUNC; $fn_name $@"
}

alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'
alias g=htop
alias b='cd -'
alias ls='ls --color=auto'
alias sls='sudo ls --color=auto'
alias lsa='ls -CFah --color=auto'
alias ll='ls -CFlh --color=auto'           # Use ll -t, ll -tr, ll -S, ll -Sr for more sorting options. Similarly for lla and lld
alias lla='ls -CFalh --color=auto'
alias slla='sudo ls -CFalh --color=auto'
lld() { ls -CFalh $* --color=force | grep -e "^d" -e total --color=never;ls -CFalh $* --color=force | grep -vE "^d|total"; }
alias lldt='ls -CFalht --color=force | grep -e "^d" -e total --color=never;ls -CFalht --color=force | grep -vE "^d|total"'
alias llds='ls -CFalhS --color=force | grep -e "^d" -e total --color=never;ls -CFalhS --color=force | grep -vE "^d|total"'
cl() { cd "$@" && lls; }

alias zs="zpool status -v"
alias zi="zpool iostat -v"
alias zl="zpool list -v"
alias zsp="zfs list -t snapshot"
alias za="zs;echo;zi;echo;zl;zsp;df -Th"
alias zss="zfs list -t snapshot"
alias zssc="sudo zfs snapshot"
alias zssd="sudo zfs destroy"

alias bydl="yt-dlp -o '%(title)s [%(upload_date>%Y.%m.%d)s][%(channel)s][%(id)s].%(ext)s' --no-mtime --embed-metadata --no-embed-info-json --extractor-args 'youtube:lang=en'"
ydl() {
    _checkargs $# 1 || return 1
    opts=()
    res="480"
    [[ "$1" == "r" ]] && { res="$2"; shift; shift; }
    [[ "$1" == "ad" ]] && { opts+=(-f 'bestaudio' -x --audio-format opus --audio-quality 0 --embed-metadata --no-embed-info-json --embed-thumbnail); shift; } || opts+=(-f "bestvideo[height<=$res]+bestaudio/best[height<=$res]")
    [[ "$1" == "ws" ]] && { opts+=(--write-subs --write-auto-subs -o 'subtitle:ysubs/%(title)s [%(upload_date>%Y)s][%(channel)s][%(id)s].%(ext)s' --convert-subs 'srt' --compat-options 'no-live-chat'); shift; }
    [[ "$1" == "es" ]] && { opts+=(--embed-subs); shift; }
    bydl "${opts[@]}" "$@"
}
alias df='df -Th'
alias lsg='ls -CFalh --color=auto | grep --color=auto -i'
alias psg='ps aux | grep -v grep | grep -i -e VSZ -e '
alias psgc='ps aux | grep -v grep | grep -i -e '
alias pkl='kill -9'
alias spkl='sudo kill -9'
alias portso='netstat -tulnp | grep LISTEN | sort -k6'
alias portsa='netstat -tulanp'
ports() {
    option="${1:-b}"
    pos="${2:-5}"
    if [[ "$option" == "b" ]];then
        ports u "$pos"
        echo
        ports t "$pos"
        return 0
    fi
    echo -e "Proto Recv-Q Send-Q LocalAddress Port ForeignAddress PID ProgramName\n$(netstat -lnp${option} | tail -n +3 | sed -r -e "s/LISTEN(.*)/\1 LISTEN/" -e "s|:([0-9]+) | \1 |" -e "s|/| |" | sort -n -k${pos})" | column -t
}
alias rm='rm -v'
alias rmd='rmdir -v'
alias rmr='rm -vrf'
alias srmr='sudo rm -vrf'
alias mount='mount -v'
alias umount='umount -v'
alias dmesg='dmesg --human -T'
alias grep='grep -i --color=auto'
alias tn='tail -n'
alias hn='head -n'
alias tf='tail -F'
alias df='df -h'
alias du='du -sh'
alias lsblk='lsblk -o NAME,FSTYPE,SIZE,RO,MOUNTPOINT,LABEL,UUID'
rdl() {
    dir="${1:-.}"
    readlink -f $dir
}
alias vi='vim'
alias se='sudoedit'
alias sv='sudo vim -u ~/.vimrc'
alias e='vim'
alias ee='emacsclient -c'
alias dateh='date --help|sed -n "/^ *%%/,/^ *%Z/p"|while read l;do F=${l/% */}; date +%$F:"|'"'"'${F//%n/ }'"'"'|${l#* }";done|sed "s/\ *|\ */|/g" |column -s "|" -t'
alias xcp='xclip -selection clipboard'
alias httpserver="python3 -m http.server"
alias sx="startx"
alias rcc="rclone check ~/Zotero-ipad gdrive-zotero:Tablet"
alias rcid="rclone sync --progress --fast-list --drive-use-trash=false --max-delete 0 gdrive-zotero:Tablet ~/Zotero-ipad"
alias rciu="rclone sync --progress --fast-list --drive-use-trash=false --immutable --retries 1 --max-delete 0 ~/Zotero-ipad gdrive-zotero:Tablet"
alias rciudd="rclone sync --progress --fast-list --drive-use-trash=false --retries 1 ~/Zotero-ipad gdrive-zotero:Tablet"
alias rcdu="rclone sync --progress --fast-list --drive-use-trash=false ~/Zotero/ gdrive-zotero:Backup"

alias myips='ip -o -f inet addr | grep -v "127.0.0.1" | cut -d'/' -f1 | sed -r "s/[ \t]+/ /g" | cut -d" " -f2-4 | awk "{print \$1\": \"\$3}" | sort | uniq'
mypublicip() {
    printf "curl -s ident.me\ncurl -s icanhazip.com\ncurl -s4 ifconfig.co\ncurl -s6 ifconfig.co" | xargs -I{} sh -c 'x=$({} | tr -d "\"";echo " | {}");echo $x'
}

alias please='sudo $(fc -ln -1)'
alias prettyplease='sudo $(history | tail -1 | awk "{\$1=\"\";print}" | xargs)'

alias rzsh='. ~/.zshrc'
alias rbash='. ~/.bashrc'

alias ga='git add'
alias gc='git commit -m'
alias gca='git commit -am'
alias gcm='git commit --amend'
alias gcma='git commit --amend -a'
alias gs="git ls -n5;echo;git status"
alias gsa="git lsa -n5;echo;git status"
alias gt='git stash'
alias gtp='git stash pop'
alias gcl='git clone'
alias gb='git branch -a'
alias gpu='git push'
alias gp='git pull'
alias gpp='git pull --rebase && git push'
alias gcf='git config --list'
alias grh='git reset HEAD'
alias grc='git rebase --continue'
alias gra='git rebase --abort'
alias gd="git diff"
alias gk="gitk --all"
alias gg="git gui"

alias dk='sudo docker'
alias dkr='sudo docker run'
alias dki='sudo docker images'
alias dkia='sudo docker images -a'
alias dkc='sudo docker ps'
alias dkca='sudo docker ps -a'
dkrc() { sudo docker start $1 && sudo docker attach $1;}
dkrm() { sudo docker kill $@; sudo docker rm $@; }
alias isolate="sudo docker network disconnect bridge"
alias join="sudo docker network connect bridge"

# Pacman package management
alias pcm='pacman'
alias pcmu='sudo pacman -Syu --needed'
alias pcmi='sudo pacman -S --needed'
alias pcms='pacman -Ss'
alias pcmsl='pacman -Qs'
alias pcmr='sudo pacman -Rcs'
alias pcmri='sudo pacman -Rc'
alias pcmc='sudo pacman -Sc --noconfirm'
alias pcmm='pacman -Qm'
alias pcml='pacman -Ql'
alias pcmo='pacman -Qo'
pcmwo() {
    wch="$(which $1 2>/dev/null)"
    echo $wch
    [[ "$wch" =~ ^/.* ]] && pcmo "$wch"
}
alias pcmii='pacman -Qi'

alias pru='pikaur -Syu --needed --mflags=--skippgpcheck'
alias prua='pru --devel'
alias pri='pikaur -S --needed'
alias prs='pikaur -Ss'

add_to_repo() {
    _checkargs $# 2 || return 1
    [[ ! -r PKGBUILD ]] && echo "Must be run in a PKGBUILD dir" && return 1
    root=$1
    repo_db=$2
    source PKGBUILD
    pkgver="$pkgver-$pkgrel"
    [[ "$MYSHELL" = 'bash' ]] && shopt -s failglob
    for i in *$pkgver*tar*;do
        x=$(echo $i | sed -r "s/(.*)-$pkgver.*/\1/")
        sudo rm -v $root/$x* 2> /dev/null
        sudo cp -v $i $root/$i
        sudo repo-add $root/$repo_db $i
    done
}

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

alias osv='cat /etc/*-release /etc/debian_version 2>/dev/null | sort | uniq | xargs -L1'
tfm() {
    n=${1:-30}
    sudo journalctl -n${n} -f
}
alias magic2='cd;~/.dotfiles/scripts/startOpenVPN.sh ~/directi/client.ovpn `~/sshhhh mnetu | base64 --decode` `~/sshhhh mnetp | base64 --decode` `~/sshhhh mnetc | base64 --decode | python2 ~/.dotfiles/scripts/gauthenticator.py`'
alias magic='cd;~/.dotfiles/scripts/startOpenVPN.sh ~/directi/mnet-client.ovpn `~/sshhhh mnetu | base64 --decode` `~/sshhhh mnetp | base64 --decode` `~/sshhhh mnetc2 | base64 --decode | python2 ~/.dotfiles/scripts/gauthenticator.py`'

xsshlistener() {
    [[ $# -lt 2 ]] && echo "Inputs missing!" && return 1
    host=$1
    shift
    str=""
    for i in "$@";do str="$str -L ${i}:localhost:${i}"; done
    cmd="ssh -o ServerAliveInterval=60 -fN $str $host"
    bash -c "$cmd"
}

xgen2() {
    _checkargs $# 1 || return 1
    x="$(printf "%d\n" \'${1: -1})"
    y=${#1}
    echo "$1$((x+5))$((y+5))"
}

xgen() {
    _checkargs $# 1 || return 1
    f="$(printf "%d\n" \'${1:0:1})"
    l="$(printf "%d\n" \'${1: -1})"
    y=${#1}
    t=$((f+l-96*2))
    echo "$1$((t+t%y))"
}

printColors() {
    echo $(for code ({0..255}) print -P -- "%F{$code}$code %f")
    for C in {0..255}; do tput setab $C; echo -n "$C "; done; tput sgr0; echo
}

# Taken from http://jeroenjanssens.com/2013/08/16/quickly-navigate-your-filesystem-from-the-command-line.html
export MARKPATH=$HOME/.local/share/marks
mkdir "$MARKPATH" &>/dev/null
function j {
    cd -P "$MARKPATH/${1:-d}" 2>/dev/null || echo "No such mark: $1"
}
function m {
    _checkargs $# 1 || return 1
    mkdir -p "$MARKPATH"; ln -s "$(pwd)" "$MARKPATH/$1" 2>/dev/null || echo "Could not mark"
}
function mf {
    _checkargs $# 1 || return 1
    mkdir -p "$MARKPATH"; ln -snf "$(pwd)" "$MARKPATH/$1"
}
function um {
    _checkargs $# 1 || return 1
    bash -c "cd $MARKPATH && rm -v $1"
}
function mks {
    ls -n "$MARKPATH" | grep -v total | tr -s ' ' | cut -d ' ' -f 9- | sed 's/->/:/' | column -t -s ':'
}
if [[ $- = *i* ]] && [[ "$MYSHELL" = 'zsh' ]];then
    function _completemarkszsh {
        reply=($(ls $MARKPATH))
    }
    compctl -K _completemarkszsh j
    compctl -K _completemarkszsh um
fi
if [[ $- = *i* ]] && [[ "$MYSHELL" = 'bash' ]];then
    _completemarksbash() {
        local curw=${COMP_WORDS[COMP_CWORD]}
        local wordlist=$(find $MARKPATH -type l -printf "%f\n")
        COMPREPLY=($(compgen -W '${wordlist[@]}' -- "$curw"))
        return 0
    }
    complete -F _completemarksbash j um
fi

xsendkeys() {
    _checkargs $# 1 || return 1
    tmux list-windows | grep -v active | cut -d: -f1 | sort -n -r | xargs -I{} bash -c "echo {} && tmux send-keys -t :{} $(echo $1 | sed 's/ / SPACE /g')"
}

xmultispawn() {
    option=$1; shift
    max="$1"; shift
    [[ $max -le 0 ]] && echo "max should be >= 1" && return
    name="$1"; shift
    { [[ -z "$name" ]] || tmux list-windows | awk '{print $2}' | tr 'A-Z' 'a-z' | tr -dc 'a-z\n' | grep "^$name$" >/dev/null 2>&1; } && name=$(< /dev/urandom tr -dc "a-z" | head -c3) || true

    case "$option" in
        v) layout=even-vertical
        ;;
        h) layout=even-horizontal
        ;;
        hv) layout=tiled
        ;;
        *) echo 'Usage: xmultispawn h/v/hv max_pane_per_window windowname "ssh c8-logging-"{1..3}' && return;;
    esac

    total=$#
    echo "Total = $total"
    [[ $total -eq 0 ]] && return

    totalwindows=$(echo "($total+$max-1)/$max" | bc)
    echo "Total windows = $totalwindows"
    adjust=$((total%totalwindows))

    for i in $(eval echo "{1..$totalwindows}");do
        paneperwindow=$((total/totalwindows)); [[ $adjust -gt 0 ]] && paneperwindow=$((paneperwindow+1)) && adjust=$((adjust-1))

        echo "Spawning window $i"
        tmux neww -dn ${name}-${i} "$1"; shift
        sleep 0.3

        paneperwindow=$((paneperwindow-1))
        [[ $paneperwindow -lt 1 ]] && continue
        c=h
        for j in $(eval echo "{1..$paneperwindow}");do
            tmux splitw -$c -t ${name}-${i}.$j -d "$1"; shift
            [[ "$c" = "h" ]] && c=v || c=h
        done
        tmux select-layout -t ${name}-${i} $layout
        tmux set-window-option -t ${name}-${i} synchronize-panes on
    done
}

alias ccm='sudo ccm64'
alias xchromestart="chromium --proxy-server='socks://127.0.0.1:9999' --incognito"

# Systemd service management
sds() { sudo systemctl status -l --no-pager -n10 $1; }
sdsf() { sudo systemctl status -l --no-pager -n0 $1; echo; sudo journalctl -f -u $1 -S "$2"; }
sdst() { dt=$(date +'%a %Y-%m-%d %T %Z'); sudo systemctl start $1; sdsf $1 "$dt"; }
sdsp() { dt=$(date +'%a %Y-%m-%d %T %Z'); sudo systemctl stop $1; sdsf $1 "$dt"; }
sdr() { dt=$(date +'%a %Y-%m-%d %T %Z'); sudo systemctl restart $1; sdsf $1 "$dt"; }
sde() { sudo systemctl enable $1; ls -l /etc/systemd/system/multi-user.target.wants; }
sdd() { sudo systemctl disable $1; ls -l /etc/systemd/system/multi-user.target.wants; }

# User service management
sus() { systemctl --user status -l --no-pager -n10 $1; }
susf() { systemctl --user status -l --no-pager -n0 $1; echo; journalctl --user -f -u $1 -S "$2"; }
sust() { dt=$(date +'%a %Y-%m-%d %T %Z'); systemctl --user start $1; susf $1 "$dt"; }
susp() { dt=$(date +'%a %Y-%m-%d %T %Z'); systemctl --user stop $1; susf $1 "$dt"; }
sur() { dt=$(date +'%a %Y-%m-%d %T %Z'); systemctl --user restart $1; susf $1 "$dt"; }
sue() { systemctl --user enable $1; ls -l /home/$USER/.config/systemd/user/*; }
sud() { systemctl --user disable $1; ls -l /home/$USER/.config/systemd/user/*; }

# Init scripts service management
ups() { sudo service $1 status ; }
upst() { sudo service $1 start ; }
upsp() { sudo service $1 stop ; }
upr() { sudo service $1 restart ; }
uprl() { sudo service $1 reload ; }
upe() { sudo chkconfig --add $1 && sudo chkconfig $1 on && sudo chkconfig --list $1 ; }
upd() { sudo chkconfig $1 off && sudo chkconfig --list $1 ; }

hold_fort() {
    while true;do
        date
        xdotool getmouselocation --shell
        echo
        xdotool mousemove_relative -- $(( $RANDOM % 30 - 15)) $(( $RANDOM % 30 - 15));xdotool getmouselocation --shell
        echo
        sleep 55
    done
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

    if command -v rg &> /dev/null;then
        rg $@
    else
        grep -R $@ .
    fi

}

xf() {
    if [ $# -lt 1 ]; then
        echo "No input!"
        return 1
    fi

    if command -v fd &> /dev/null;then
        fd $@
    else
        find -iname "*$**"
    fi
}

h() { if [ -z "$*" ]; then history 1; else history 1 | grep -E "$@"; fi; }

alias jp="japanesec"
japanese() {
    if [ $# -lt 1 ]; then
        echo "No input!"
        return 1
    fi

    python ~/.dotfiles/scripts/japanese-get-kana.py "$@"
}

japanesec() {
	if [ $# -lt 1 ]; then
        echo "No input!"
        return 1
    fi

    p=$(japanese "$@")
    echo $p
    echo -n $p | xcp
    echo "Copied to clipboard"
}

rand() {
    python ~/.dotfiles/scripts/password.py "$@"
}

randc() {
    rand "$@" | tr -d '\n' | xcp
    echo "Copied to clipboard"
}

randp() {
    password=$(rand "$@" | tr -d '\n')
    echo "${#password} $password"
    echo -n "$password" | xcp
    echo "Copied to clipboard"
}

up() {
    if [ -z "$*" ]; then 1='1';fi
    pd=`pwd`
    cd $(eval printf '../'%.0s {1..$1}) && echo "${pd} -> $(pwd)";
}

fawk() {
    if [ $# -lt 1 ]; then
        echo "No input!"
        return 1
    fi
    [[ -n "$2" ]] && local sep="-F$2"
    local first="awk $sep '{print "
    local last="}'"
    local cmd="${first}\$${1}${last}"
    eval $cmd
}

pkla() {
    if [ $# -lt 1 ]; then
        echo "No input!"
        return 1
    fi
    ps aux | grep -v grep | grep -i -e $1 | awk '{print $2}' | xargs kill -9
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
        for i in $(cat ~/sshhhh | grep ')' | grep -v '*' | grep -v 'mnet' | cut -f1 -d')' | xargs);do
            printf "$i "
            code=$(~/sshhhh "$i" | base64 --decode | python2 ~/.dotfiles/scripts/gauthenticator.py)
            printf $code
            echo
        done
    else
        code=$(~/sshhhh "$1" | base64 --decode | python2 ~/.dotfiles/scripts/gauthenticator.py)
        printf $code | xclip -selection clipboard
        echo "Copied $code to clipboard"
    fi
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
		[[ -d "$i/.git" ]] && { [[ -n "$(git --git-dir="$i/.git" --work-tree="$i" log --since="$dayz day ago" --pretty=oneline | grep -v SVN_SILENT)" ]] && cd $i && echo $i && { gitk --since="$dayz day ago" || true; } && cd .. ; }
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
