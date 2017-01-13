HISTFILE=~/.bash_history
HISTSIZE=100000
SAVEHIST=$HISTSIZE

autoload -U colors && colors
autoload -U compinit
compinit -i
zstyle ':completion:*' rehash yes
autoload -U promptinit
promptinit
prompt walters

. ~/zsh-git-prompt/zshrc.sh 2>/dev/null
GIT_PROMPT_EXECUTABLE="haskell"
MYSHELL=$(ps -p $$ -ocomm= 2>/dev/null)
[[ $UID -eq 0 ]] && color=red || color=green
NEWLINE=$'\n'
PROMPT="${NEWLINE}[%{$fg_bold[blue]%}${MYSHELL}%{$reset_color%}:%{$fg_bold[${color}]%}%n@%M%{$reset_color%}][%D{%H:%M:%S}] %{$fg_bold[yellow]%}%~ \$(git_super_status 2>/dev/null)${NEWLINE}%(?.%{$fg[green]%}.%{$fg[red]%})%#%{$reset_color%} "
RPROMPT=""

bindkey -e
bindkey \^U backward-kill-line
bindkey "^[[1~" beginning-of-line
bindkey "^[[4~" end-of-line
bindkey "^[[3~" delete-char

autoload -U up-line-or-beginning-search
autoload -U down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
bindkey '\eOA' up-line-or-beginning-search
bindkey '\e[A' up-line-or-beginning-search
bindkey '\eOB' down-line-or-beginning-search
bindkey '\e[B' down-line-or-beginning-search

setopt nobeep \
    notify \
    nobgnice \
    correct \
    interactivecomments \
    autocd \
    autopushd \
    pushdtohome \
    chaselinks \
    histverify \
    histappend \
    sharehistory \
    hist_reduce_blanks \
    hist_ignore_space \
    hist_ignore_all_dups \
    hist_save_no_dups \
    braceccl \
    dotglob \
    extendedglob \
    numericglobsort \
    nolisttypes \
    promptsubst \
    completealiases
