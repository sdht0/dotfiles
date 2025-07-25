declare -xr DOTFILES=~/.config/dotfiles
[[ "$(uname -o)" == "Darwin" ]] && declare -xr IS_DARWIN="1"

# Replace with a tmux session if it is an interactive session and tmux is installed and is not already running
if [[ $UID -ne 0 ]] && [[ $- = *i* ]] && [[ -t 1 ]] && command -v tmux &>/dev/null 2>&1 && [[ -z "$TMUX" ]] && [[ -z "$NOTMUX" ]] ;then
    ID="$(tmux ls | grep -vm1 attached | cut -d: -f1)" # get the id of a deattached session
    if [[ -z "$ID" ]] ;then # if not available create a new one
        exec tmux new-session
    else
        exec tmux attach-session -t "$ID" # if available attach to it
    fi
fi

export EDITOR='vim'
export VISUAL=$EDITOR
declare -xr HISTFILESIZE=100000
declare -xr HISTSIZE=${HISTFILESIZE}
[[ -f ${DOTFILES}.safe/bash_history ]] && declare -xr HISTFILE=${DOTFILES}.safe/bash_history || declare -xr HISTFILE=~/.bash_history
declare -xr SAVEHIST=$HISTSIZE
PROMPT_COMMAND="history -a; $PROMPT_COMMAND"            # Write history immediately

MYSHELL=$(ps -p $$ -ocomm= 2>/dev/null)

#[[ $- = *i* ]] && stty -ixon
if [[ $- = *i* ]] && [[ "$MYSHELL" = 'bash' ]];then

    if [[ -f /etc/bashrc ]];then
        . /etc/bashrc
    fi

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
    export PS1="\n[${BBlue}${MYSHELL}${Color_Off}:${color}\u@\H${Color_Off}][\t] ${BGreen}\w\n\$([[ \$? == 0 ]] && echo \"${BGreen}\" || echo \"${BRed}\")${prompt}${Color_Off} "

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
    local fn_name="$1"
    shift
    local FUNC
    FUNC="$(declare -f "$fn_name")"
    [[ -z "$FUNC" ]] && { echo "Error: Function '$fn_name' not found"; return; }
    sudo bash -c "$FUNC; $fn_name $*"
}

alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'
alias h=htop
alias b='cd -'

alias p="pnpm"

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
    local opts=()
    local res="480"
    if [[ "$1" == "ad" ]];then
        opts+=(-f 'bestaudio' -x --audio-format opus --audio-quality 0 --embed-metadata --no-embed-info-json --embed-thumbnail)
        shift
    else
        [[ "$1" == "r" ]] && { res="$2"; shift; shift; }
        opts+=(-f "bestvideo[height<=$res]+bestaudio/best[height<=$res]")
    fi
    [[ "$1" == "ws" ]] && { opts+=(--write-subs --write-auto-subs -o 'subtitle:ysubs/%(title)s [%(upload_date>%Y)s][%(channel)s][%(id)s].%(ext)s' --convert-subs 'srt' --compat-options 'no-live-chat'); shift; }
    [[ "$1" == "es" ]] && { opts+=(--embed-subs); shift; }
    bydl "${opts[@]}" "$@"
}
alias ydla='ydl ad'
alias lsg='ls -CFalh --color=auto | grep --color=auto -i'
alias psg='ps aux | grep -v grep | grep -i -e "^USER " -e '
alias psgc='ps aux | grep -v grep | grep -i -e '
alias pkl='kill -9'
alias spkl='sudo kill -9'
ports() {
    if [[ -n "" ]];then
        lsof -i -nP
    else
        [[ "$1" == "" ]] && { 1="-k6,6n"; 2="-k3,3n"; 3="-k4,4"; 4="-k1,1"; }
        local sep=";"
        (
            echo -e "Proto${sep}Address${sep}Port${sep}v${sep}UID${sep}PID${sep}Process${sep}Other"
            (
                sudo ss --no-header -l4tunpe | sed -r "s/^/v4 /"
                sudo ss --no-header -l6tunpe | sed -r "s/^/v6 /"
            ) | awk '{$6=gensub(/(.+):(.+)/,"\\1'${sep}'\\2",1,$6); rest=""; for(i=8;i<=NF;i++) { rest=rest " " $i; }; if(rest ~ /uid:/) { $8=gensub(/.*uid:([0-9]+).*/,"\\1",1,rest); } else { $8="0"; }; if(rest ~ /^ users/) { $8=$8 gensub(/^ users:\(\("([^"]+)",pid=([0-9]+),(.*)\)\).*/,"'${sep}'\\2'${sep}'\\1'${sep}'\\3",1,rest); $8=gensub(/\),\(/,",","g",$8); } else { $8=$8 "'${sep}'-'${sep}'-'${sep}'-"; }; if(rest ~ /.service/) { $8=$8 "," gensub(".*/([^\\.]+\\.service).*","\\1",1,rest); }; if(rest ~ /docker.*scope/) { $8=$8 ",docker.scope"; }; OFS="'${sep}'"}{print $2,$6,$1,$8}' | sort --field-separator "${sep}" "$@"
        ) | column -t -s"${sep}"
    fi
}
alias mvv='mv -v'
alias rmv='rm -v'
alias rmd='rmdir -v'
alias rmr='rm -vrf'
alias srmr='sudo rm -vrf'
alias mountv='mount -v'
alias umountv='umount -v'
alias dmsg='dmesg --human -T'
alias grepi='grep -i --color=auto'
alias ev="env | sort"
alias tn='tail -n'
alias hn='head -n'
alias tf='tail -F'
alias dfh='df -Th'
alias duh='du -sh'
alias lblk='lsblk -o NAME,FSTYPE,SIZE,RO,MOUNTPOINT,LABEL,UUID'
rdl() {
    local dir="${1:-.}"
    readlink -f "$dir"
}
alias vi='vim'
alias se='sudoedit'
alias sv='sudo vim -u ~/.vimrc'
alias e='vim'
alias k='kwrite'
alias ee='emacsclient -c'
alias dateh='date --help|sed -n "/^ *%%/,/^ *%Z/p"|while read l;do F=${l/% */}; date +%$F:"|'"'"'${F//%n/ }'"'"'|${l#* }";done|sed "s/\ *|\ */|/g" |column -s "|" -t'
alias xcp='xclip -selection clipboard'
alias httpserver="python3 -m http.server"
alias sx="startx"
myips() {
    if [[ -n "${IS_DARWIN:-}" ]];then
        ifconfig | awk '/^[a-z]/ { iface=$1; sub(":", "", iface) } /inet / && $2 != "127.0.0.1" { print iface ": " $2 }' | sort | uniq
    else
        ip -o -f inet addr | grep -v "127.0.0.1" | cut -d'/' -f1 | sed -r "s/[ \t]+/ /g" | cut -d" " -f2-4 | awk '{print $1": "$3}' | sort | uniq
    fi
}
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
alias gco='git checkout'
alias gct='git checkout --track'
alias gs="git ls -n5;echo;git status"
alias gss="git status"
alias gsa="git lsa -n5;echo;git status"
alias gt='git stash'
alias gtp='git stash pop'
alias gcl='git clone'
alias gb='git branch'
alias gba='git branch -a'
alias gbd='git branch -D'
alias gpu='git push'
alias gp='git pull'
alias gpp='git pull --rebase && git push'
alias gcf='git config --list'
alias grh='git reset HEAD'
alias grc='git rebase --continue'
alias gra='git rebase --abort'
alias gd="git diff"
alias gds="git diff --staged"
alias gk="gitk --all"
alias gg="git gui"
alias gu="gitui"

alias dk='sudo docker'
alias dkr='sudo docker run'
alias dki='sudo docker images'
alias dkia='sudo docker images -a'
alias dkc='sudo docker ps'
alias dkca='sudo docker ps -a'
dkrc() { sudo docker start "$1" && sudo docker attach "$1";}
dkrm() { sudo docker kill "$@"; sudo docker rm "$@"; }
alias isolate="sudo docker network disconnect bridge"
alias join="sudo docker network connect bridge"

alias pm='podman'
alias pmr='podman run'
alias pmi='podman images'
alias pmia='podman images -a'
alias pmc='podman ps'
alias pmca='podman ps -a'

# Nixos
nxos() {
    local config
    local picker
    [[ -n "${IS_DARWIN:-}" ]] \
        && { config=~/.config/nix-darwin; picker="darwin"; } \
        || { config=/etc/nixos; picker="nixos"; }

    [[ "${1:-}" == "upd" ]] && { nix flake update --flake "$config"; shift; }

    if command -v dix &>/dev/null ;then
        local left="$(nix path-info --derivation "/run/current-system")" || return 3
        local right="$(nix path-info --derivation "${config}#${picker}Configurations.$(hostname -s).config.system.build.toplevel")" || return 3
        dix "$left" "$right" || return 1
        [[ "$left" == "$right" ]] && echo "No change in configuration" && return 0
        echo
    fi

    if [[ -n "${IS_DARWIN:-}" ]];then
        sudo true || return 2
        sudo darwin-rebuild --keep-going --flake "$config" "${1:-switch}"
    else
        if [[ "${1:-}" == "dry" ]];then
            local fl="/tmp/dry-build.txt"

            nixos-rebuild --flake "$config" dry-build &> $fl || return

            nix store diff-closures "$(nix path-info --derivation "/run/current-system")" "$(cat "$fl" | grep nixos-system | tr -d ' ')"
            echo
            echo "Download:"
            cat "$fl" | awk 'p;/will be fetched/{p=1}' | tr -d ' '

            echo
            echo "Local:"
            cat "$fl" | awk '/fetched/{p=0}p;/will be built/{p=1}' | while read d ;do grep -qi "preferLocalBuild" "$d" && echo "$d" ;done

            echo
            echo "Build:"
            cat "$fl" | awk '/fetched/{p=0}p;/will be built/{p=1}' | while read d ;do grep -qi "preferLocalBuild" "$d" || echo "$d" ;done

            echo
            :
        else
            local logdir=/var/log/nixos
            local logfile="$logdir/$(date +'%Y.%m.%d-%H:%M:%S').log"
            touch "$logfile" || { echo "Cannot create log file: $logfile"; return 10; }
            echo "Logging to $logfile"

            sudo true || return 2
            if command -v nom &>/dev/null ;then
                sudo nice -10 nixos-rebuild --log-format raw --keep-going --flake "$config" "${1:-boot}" |& sudo tee -a "$logfile" |& nom
            else
                sudo nice -10 nixos-rebuild --log-format raw --keep-going --flake "$config" "${1:-boot}" |& sudo tee -a "$logfile"
            fi
        fi
    fi
}
nxd() {
    _checkargs $# 1 || return 1
    local qt="$1"
    local q
    case "$qt" in
        "rf") q="--references" ;;
        "rfc") q="--requisites" ;;
        "rr") q="--referrers" ;;
        "rrc") q="--referrers-closure" ;;
        *) q="--references" ;;
    esac
    local p="$2"
    echo "$p" | grep -q ".drv$" || p="$(nix-store --query --deriver "$p")"

    nix-store --query $q "$p"
}
nxp() {
    _checkargs $# 1 || return 1
    local qt="$1"
    local q
    case "$qt" in
        "rf") q="--references"; shift ;;
        "rfc") q="--requisites"; shift ;;
        "rr") q="--referrers"; shift ;;
        "rrc") q="--referrers-closure"; shift ;;
        "dr") q="--deriver"; shift ;;
        *) q="--references" ;;
    esac
    local p="$1"
    echo "$p" | grep -q ".drv$" && { echo "Error: $p is not a path"; return 1; }

    nix-store --query $q "$p"
}
nxds() {
    _checkargs $# 1 || return 1
    local p="$1"
    echo "$p" | grep -q ".drv$" || p="$(nix-store --query --deriver "$p")"
    nix derivation show "$p^*"
}
nxwl() {
    ls -l "$(which "$1")"
}
nxr() {
    _checkargs $# 1 || return 1
    local p=$1
    shift
    nix run "nixpkgs#$p" "$@"
}
nxwd() {
    _checkargs $# 2 || return 1

    local p1="$1"
    echo "$p1" | grep -q ".drv$" || p1="$(nix-store --query --deriver "$p1")"

    local p2="$2"
    echo "$p2" | grep -q ".drv$" || p2="$(nix-store --query --deriver "$p2")"

    nix why-depends "$p1" "$p2"
}
nxdfs() {
    _checkargs $# 2 || return 1

    difft --display side-by-side-show-both <(nxds "$1") <(nxds "$2")
}
nxdf() {
    _checkargs $# 2 || return 1

    difft --override='*:json' --display side-by-side-show-both --skip-unchanged --ignore-comments --context 0 \
        <(nxds "$1" | sed -r 's|/nix/store/[^-]+-||g' | jaq --sort-keys) \
        <(nxds "$2" | sed -r 's|/nix/store/[^-]+-||g' | jaq --sort-keys)
}
nxv() {
    _checkargs $# 1 || return 1

    nix develop "/etc/nixos#$1" --command zsh
}
nxs() {
    _checkargs $# 1 || return 1

    nix-shell --command zsh -p $@
}
nxsp() {
    _checkargs $# 1 || return 1

    nix-shell --pure --command "NOTMUX=1 zsh" -p $@
}

# Pacman package management
alias pcm='pacman'
alias pcmu='sudo pacman -Syu --needed'
alias pcmi='sudo pacman -S --needed'
alias pcms='pacman -Ss'
alias pcmsl='pacman -Qs'
alias pcmr='sudo pacman -Rcs'
alias pcmri='sudo pacman -Rc'
alias pcmc='sudo pacman -Sc --noconfirm'
alias pcmm='pacman -Qm; echo; pacman -Qdt'
alias pcml='pacman -Fl'
alias pcmll='pacman -Ql'
alias pcmii='pacman -Si'
alias pcmiil='pacman -Qii'
alias pcmo='pacman -Qo'
alias pcmor='pacman -F'
pcmwo() {
    local wch="$(which "$1" 2>/dev/null)"
    echo "$wch"
    [[ "$wch" =~ ^/.* ]] && pcmo "$wch"
}
alias pru='pikaur -Syu --needed --mflags=--skippgpcheck'
alias prua='pru --devel'
alias pri='pikaur -S --needed'
alias prs='pikaur -Ss'
add_to_repo() {
    _checkargs $# 2 || return 1
    [[ ! -r PKGBUILD ]] && echo "Must be run in a PKGBUILD dir" && return 1
    local root=$1
    local repo_db=$2
    source "./PKGBUILD"
    local pkgver="$pkgver-$pkgrel"
    [[ "$MYSHELL" = 'bash' ]] && shopt -s failglob
    for i in *"$pkgver"*tar*;do
        local x=$(echo $i | sed -r "s/(.*)-$pkgver.*/\1/")
        sudo rm -v "$root/$x"* 2> /dev/null
        sudo cp -v "$i" "$root/$i"
        sudo repo-add "$root/$repo_db" "$i"
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

osv() {
    for i in /etc/*-release /etc/debian_version ;do
        [[ -f "$i" ]] && echo "> $i" && cat "$i"
    done
}
alias osva='cat /etc/*-release /etc/debian_version 2>/dev/null | sort | uniq | xargs -L1'
tfm() {
    local n=${1:-30}
    sudo journalctl -n${n} -f
}
alias magic2='cd;$DOTFILES/scripts/startOpenVPN.sh ~/directi/client.ovpn `~/sshhhh mnetu | base64 --decode` `~/sshhhh mnetp | base64 --decode` `~/sshhhh mnetc | base64 --decode | python2 $DOTFILES/scripts/gauthenticator.py`'
alias magic='cd;$DOTFILES/scripts/startOpenVPN.sh ~/directi/mnet-client.ovpn `~/sshhhh mnetu | base64 --decode` `~/sshhhh mnetp | base64 --decode` `~/sshhhh mnetc2 | base64 --decode | python2 $DOTFILES/scripts/gauthenticator.py`'

xsshlistener() {
    _checkargs $# 2 || return 1
    local host=$1
    shift
    local str=""
    for i in "$@";do
        local l="$(echo $i | cut -d: -f1)"
        local r="$(echo $i | cut -d: -f2)" # Equal to $l if no colon
        str="$str -L ${l}:localhost:${r}";
    done
    local cmd="ssh -o ServerAliveInterval=60 -fN $str $host"
    bash -c "$cmd"
}

xgen2() {
    _checkargs $# 1 || return 1
    local x="$(printf "%d\n" \'${1: -1})"
    local y=${#1}
    echo "$1$((x+5))$((y+5))"
}

xgen() {
    _checkargs $# 1 || return 1
    local f="$(printf "%d\n" \'${1:0:1})"
    local l="$(printf "%d\n" \'${1: -1})"
    local y=${#1}
    local t=$((f+l-96*2))
    echo "$1$((t+t%y))"
}

printColors() {
    echo $(for code in {0..255};do print -P -- "%F{$code}$code %f" ;done)
    for C in {0..255}; do tput setab $C; echo -n "$C "; done; tput sgr0; echo
}

# Taken from http://jeroenjanssens.com/2013/08/16/quickly-navigate-your-filesystem-from-the-command-line.html
MARKPATH=${DOTFILES}.safe/marks
function j {
    mkdir -p "$MARKPATH"
    local mark="$MARKPATH/${1:-d}"
    [[ -L "$mark" ]] || { echo "No such mark: ${1:-d}"; return 1; }
    local dest="$(readlink "$mark")"
    [[ -e "$dest" ]] || { echo "Destination missing: $1 -> $dest"; return 1; }
    cd -P "$dest"
}
function m {
    _checkargs $# 1 || return 1
    mkdir -p "$MARKPATH"
    local mark="$MARKPATH/$1"
    [[ -L "$mark" ]] && { echo "Mark exists: $1 -> $(readlink "$mark")"; return 1; }
    echo "Marked: $1 -> $(pwd)" && ln -s "$(pwd)" "$mark"
}
function mf {
    _checkargs $# 1 || return 1
    mkdir -p "$MARKPATH"
    local mark="$MARKPATH/$1"
    [[ -L "$mark" ]] && { echo "Replacing $1: $(readlink "$mark") -> $(pwd)"; rm "$mark"; }
    m "$1"
}
function um {
    _checkargs $# 1 || return 1
    mkdir -p "$MARKPATH"
    local mark="$MARKPATH/$1"
    [[ -L "$mark" ]] || { echo "No such mark: $1"; return 1; }
    (cd "$MARKPATH" && echo "Removing mark: $1 -> $(readlink "$mark")" && \rm "$1" )
}
function mks {
    mkdir -p "$MARKPATH"
    ls -n "$MARKPATH" | grep -v total | tr -s ' ' | cut -d ' ' -f 9- | sed 's/->/:/' | column -t -s ':'
}
if [[ $- = *i* ]] && [[ "$MYSHELL" = 'zsh' ]];then
    function _completemarkszsh {
        mkdir -p "$MARKPATH"
        reply=($(ls "$MARKPATH"))
    }
    compctl -K _completemarkszsh j
    compctl -K _completemarkszsh um
fi
if [[ $- = *i* ]] && [[ "$MYSHELL" = 'bash' ]];then
    _completemarksbash() {
        mkdir -p "$MARKPATH"
        local curw=${COMP_WORDS[COMP_CWORD]}
        local wordlist="$(find "$MARKPATH" -type l -printf "%f\n")"
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
    local option=$1; shift
    local max="$1"; shift
    [[ $max -le 0 ]] && echo "max should be >= 1" && return
    local name="$1"; shift
    { [[ -z "$name" ]] || tmux list-windows | awk '{print $2}' | tr '[:upper:]' '[:lower:]' | tr -dc 'a-z\n' | grep "^$name$" >/dev/null 2>&1; } && name=$(< /dev/urandom tr -dc "[:lower:]" | head -c3) || true

    local layout
    case "$option" in
        v) layout=even-vertical
        ;;
        h) layout=even-horizontal
        ;;
        hv) layout=tiled
        ;;
        *) echo 'Usage: xmultispawn h/v/hv max_pane_per_window windowname "ssh c8-logging-"{1..3}' && return;;
    esac

    local total=$#
    echo "Total = $total"
    [[ $total -eq 0 ]] && return

    local totalwindows=$(echo "($total+$max-1)/$max" | bc)
    echo "Total windows = $totalwindows"
    local adjust=$((total%totalwindows))

    for i in $(eval echo "{1..$totalwindows}");do
        local paneperwindow=$((total/totalwindows)); [[ $adjust -gt 0 ]] && paneperwindow=$((paneperwindow+1)) && adjust=$((adjust-1))

        echo "Spawning window $i"
        tmux neww -dn "${name}-${i}" "$1"; shift
        sleep 0.3

        paneperwindow=$((paneperwindow-1))
        [[ $paneperwindow -lt 1 ]] && continue
        local c=h
        for j in $(eval echo "{1..$paneperwindow}");do
            tmux splitw -$c -t "${name}-${i}.$j" -d "$1"; shift
            [[ "$c" = "h" ]] && c=v || c=h
        done
        tmux select-layout -t "${name}-${i}" $layout
        tmux set-window-option -t "${name}-${i}" synchronize-panes on
    done
}

alias ccm='sudo ccm64'
alias xchromestart="chromium --proxy-server='socks://127.0.0.1:9999' --incognito"

# System service management
sds() { sudo systemctl status -l --no-pager -n10 "$1"; }
sdsf() { sudo systemctl status -l --no-pager -n0 "$1"; echo; sudo journalctl -f -u "$1" -S "$2"; }
sdst() { local dt=$(date +'%Y-%m-%d %T'); sudo systemctl start "$1"; sdsf "$1" "$dt"; }
sdsp() { local dt=$(date +'%Y-%m-%d %T'); sudo systemctl stop "$1"; sdsf "$1" "$dt"; }
sdr() { local dt=$(date +'%Y-%m-%d %T'); sudo systemctl restart "$1"; sdsf "$1" "$dt"; }
sde() { sudo systemctl enable "$1"; ls -l /etc/systemd/system/multi-user.target.wants; }
sdd() { sudo systemctl disable "$1"; ls -l /etc/systemd/system/multi-user.target.wants; }

# User service management
sus() { systemctl --user status -l --no-pager -n10 "$1"; }
susf() { systemctl --user status -l --no-pager -n0 "$1"; echo; journalctl --user -f -u "$1" -S "$2"; }
sust() { local dt=$(date +'%Y-%m-%d %T'); systemctl --user start "$1"; susf "$1" "$dt"; }
susp() { local dt=$(date +'%Y-%m-%d %T'); systemctl --user stop "$1"; susf "$1" "$dt"; }
sur() { local dt=$(date +'%Y-%m-%d %T'); systemctl --user restart "$1"; susf "$1" "$dt"; }
sue() { systemctl --user enable "$1"; ls -l /home/"$USER"/.config/systemd/user/*; }
sud() { systemctl --user disable "$1"; ls -l /home/"$USER"/.config/systemd/user/*; }

hold_fort() {
    while true;do
        date
        xdotool getmouselocation --shell
        echo
        xdotool mousemove_relative -- $(( RANDOM % 30 - 15)) $(( RANDOM % 30 - 15));xdotool getmouselocation --shell
        echo
        sleep 55
    done
}

mkcd() {
    local dir="$*"
    mkdir -vp "$dir" && cd "$dir"
}

ctg() {
    cat "$1" | grep "$2"
}

l() {
    unset long all dir
    [[ "$1" == "_long" ]] && local long="true" && shift
    [[ "$1" == "_all" ]] && local all="true" && shift
    [[ "$1" == "_dir" ]] && local dir="true" && shift
    [[ "$1" == "_sudo" ]] && local pre="sudo " && shift

    local args=()

    if command -v eza &>/dev/null ;then
        [[ "${long:-}" == "true" ]] && args+=(-lg --icons)
        [[ "${all:-}" == "true" ]] && args+=("-aa")
        [[ "${dir:-}" == "true" ]] && args+=("--only-dirs")
        eval "${pre:-}" eza --group-directories-first --color=auto --sort=extension "${args[@]}" "$@"
    else
        [[ "${long:-}" == "true" ]] && args+=("-l")
        if [[ -n "${IS_DARWIN:-}" ]];then
            [[ "${all:-}" == "true" ]] && args+=("-A")
            eval "${pre:-}" ls -F --color=auto "${args[@]}" "$@"
        else
            [[ "${all:-}" == "true" ]] && args+=("--all")
            eval "${pre:-}" ls -XF --color=auto --group-directories-first "${args[@]}" "$@"
        fi
    fi
}
alias la='l _all'
alias ll='l _long _all'
alias sll='l _long _all _sudo'
alias ld='ll _dir'

xs() {
    _checkargs $# 1 || return 1

    [[ "$1" == "___strict___" ]] && strict="true" && shift

    local args
    if command -v rg &> /dev/null;then
        [[ "${strict:-}" == "true" ]] && args="-s" || args="-i"
        rg "${args:-}" "$@"
    else
        [[ "${strict:-}" != "true" ]] && args="-i"
        grep -ER "${args:-}" "$@" .
    fi
}
xss() {
    _checkargs $# 1 || return 1
    xs "___strict___" "$@"
}

xf() {
    _checkargs $# 1 || return 1

    [[ "$1" == "___strict___" ]] && strict="true" && shift

    if command -v fd &> /dev/null;then
        [[ "${strict:-}" == "true" ]] && args="-s" || args="-i"
        fd -j1 ${args:-} "$@"
    else
        [[ "${strict:-}" != "true" ]] && args="-iname"
        find ${args:-} "*$**"
    fi
}
xfs() {
    _checkargs $# 1 || return 1
    xf "___strict___" "$@"
}

hh() { if [ -z "$*" ]; then history 1; else history 1 | grep -E "$@"; fi; }

alias jp="japanesec"
japanese() {
    _checkargs $# 1 || return 1

    python $DOTFILES/scripts/japanese-get-kana.py "$@"
}

japanesec() {
    _checkargs $# 1 || return 1

    local p=$(japanese "$@")
    echo "$p"
    echo -n "$p" | xcp
    echo "Copied to clipboard"
}

rand() {
    python $DOTFILES/scripts/password.py "$@"
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
    local pd="$(pwd)"
    cd "$(eval printf '../%.0s' "{1..$1}")" && echo "${pd} -> $(pwd)";
}

fawk() {
    _checkargs $# 1 || return 1

    [[ -n "$2" ]] && local sep="-F$2"
    local first="awk $sep '{print "
    local last="}'"
    local cmd="${first}\$${1}${last}"
    eval "$cmd"
}

pkla() {
    _checkargs $# 1 || return 1
    ps aux | grep -v grep | grep -i -e $1 | awk '{print $2}' | xargs kill -9
}

gitkf() {
  gitk_follow () {
    while (( "$#" )); do
      git log -p --oneline --name-status --follow $1;
      shift;
    done | perl -ne 'if( s{^(?:[ACDMRTUXB]|R\d+)\s+}{} ) { s{\s+}{\n}g; print; }' | sort -u
  }
  gitk "$(gitk_follow "$@")"
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
    local os="$1"
    local nhostname="$2"

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
    _checkargs $# 1 || return 1

    # make sure we're at the root of git repo
    if [ ! -d .git ]; then
        echo "Error: must run this script from the root of a git repository"
        exit 1
    fi

    # remove all paths passed as arguments from the history of the repo
    local files=$@
    git filter-branch --index-filter "git rm -rf --cached --ignore-unmatch $files" HEAD

    # remove the temporary history git-filter-branch otherwise leaves behind for a long time
    rm -rf .git/refs/original/ && git reflog expire --all &&  git gc --aggressive --prune
}

xreauthorgit() {
    _checkargs $# 3 || return 1

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
            *.tar.zst) tar --zst -xf "$1" ;;
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
        local mstatus="updates: $(date +'%Y-%m-%d %H:%M:%S')"
        printf "You've not set any message: '$mstatus' is being used [Y|n]: "
        read inp
        if [ "$inp" = "n" ]; then
            return 1
        fi
    else
        local mstatus="$1"
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
    local f="resized"
    local n=$1
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
    _checkargs $# 1 || return 1

    local isoname=sdh-$(basename "$1")
    local isolabel=$(isoinfo -d -i "$1" | grep "Volume id" | sed "s/Volume id: //")
    local outputdir=$(pwd)
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
    mv /tmp/sdh-customiso/"$isoname" "$outputdir"
    echo "Cleaning up..."
    sudo umount /tmp/sdh-customiso/mnt
    #rm -rf /tmp/sdh-customiso
    cd "$outputdir" || return 1
    echo "Done."
}

xget() {
    if [ $# -lt 1 ]; then
        for i in $(cat ~/sshhhh | grep ')' | grep -v '*' | grep -v 'mnet' | cut -f1 -d')' | xargs);do
            echo "$i "
            local code=$(~/sshhhh "$i" | base64 --decode | python2 $DOTFILES/scripts/gauthenticator.py)
            echo "$code"
            echo
        done
    else
        local code=$(~/sshhhh "$1" | base64 --decode | python2 $DOTFILES/scripts/gauthenticator.py)
        echo "$code" | xclip -selection clipboard
        echo "Copied $code to clipboard"
    fi
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
    _checkargs $# 1 || return 1

    cd "$1" || return 1

    if [ "$2" == "" ];then
        local ext="mp4"
    else
        local ext=$2
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
    local dayz=${1:-7}
	for i in *;do
		[[ -d "$i/.git" ]] && { [[ -n "$(git --git-dir="$i/.git" --work-tree="$i" log --since="$dayz day ago" --pretty=oneline | grep -v SVN_SILENT)" ]] && cd $i && echo $i && { gitk --since="$dayz day ago" || true; } && cd .. ; }
	done
}

xpatternrename() {
    if [ $# -lt 2 ]; then
        echo "Please give two patterns!"
        return 1
    fi
    for i in *;do x="$(echo "$i" | sed "s|$1|$2|")"; if [ ! -r "$x" ];then echo "$i->$x"; mv "$i" "$x";fi;done
}

xgpp() {
    _checkargs $# 1 || return 1

    if [ $# -eq 2 ]; then
        g++ "$1" && ./a.out < "$2"
    else
        g++ "$1" && ./a.out
    fi

    rm -f a.out
}

xgcc() {
    _checkargs $# 1 || return 1

    if [ $# -eq 2 ]; then
        gcc "$1" && ./a.out < "$2" && rm a.out
    else
        gcc "$1" && ./a.out  && rm a.out
    fi
}

xccreatefolder() {
    _checkargs $# 1 || return 1

    mkdir -p "$1" && ( echo '#include<stdio.h>
int main() {

    return 0;
}' > "$1/$1.c" ; touch "$1/in$1.txt" )
}

xcppcreatefolder() {
    _checkargs $# 1 || return 1

    mkdir -p "$1" && ( echo '#include<iostream>
using namespace std;
int main() {

    return 0;
}' > "$1/$1.cpp" ; touch "$1/in$1.txt" )
}

xregexrename() {
    _checkargs $# 1 || return 1

    for i in *;do
        local x=$(echo "$i" | sed -r "$1"); [[ ! -r "$x" ]] && echo "$i -> $x" && [[ "$2" = "m" ]] && mv "$i" "$x"
    done
}

xrenametotitlecase() {
    local pd="$(pwd)"
    if [ -n "$1" ];then
        local pd="$1"
    fi
    pushd "$pd" > /dev/null && \
    for i in *;do local x=$(echo "$i" | tr '[:upper:]' '[:lower:]' | sed "s/\( \| \?-\| \?(\| \?\[\|^\)\(.\)/\1\u\2/g");[ ! -r "$x" ] && mv "$i" "$x";done && \
    popd > /dev/null
}

xrenametolowercase() {
    local depth=0
    local maxdepth=1
    rename() {
        if [ -n "$1" ];then
            echo "Inside directory: $1"
            pushd "$1" > /dev/null || true
            for i in *;do
                    if [ -d "$i" ];then
                        if [[ "$i" = "System Volume Information" || "$i" = "\$RECYCLE.BIN" ]];then
                            echo "Skipping $i"
                            continue
                        fi
                        local dr=$i
                        local x=$(echo "$i"  |tr '[:upper:]' '[:lower:]' |tr ' ' '-')
                        if [ ! -d "$x" ];then
                            echo "Dir - $i: ### Renaming to $x ###"
                            local dr=$x
                            mv "$i" "$x"
                        elif [ "$x" != "$i" ];then
                            echo "Dir - $i: *** Not renamed: dir $x already exists ***"
                        else
                            echo "Dir - $i: OK"
                        fi
                        if test $depth -lt "$maxdepth";then
                            echo $((depth++))
                            rename "$1/$dr"
                            echo $((depth--))
                        fi
                    fi
            done
            for i in *;do
                if [ -f "$i" ];then
                    local x=$(echo "$i"  |tr '[:upper:]' '[:lower:]' |tr ' ' '-')
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
            popd > /dev/null || true
        fi
    }
    local pd=$(pwd)
    if [ -n "$1" ];then
        local pd=$1
    fi
    if [ -n "$2" ] && [ "$2" = "-d" ]
        then
        if [ -n "$3" ];then
            local maxdepth=$3
        fi
    fi
    rename "$pd"
}
