HISTFILE=~/.bash_history
HISTSIZE=10000
SAVEHIST=$HISTSIZE

autoload -U colors && colors
autoload -U compinit
compinit -i
zstyle ':completion:*' rehash yes
autoload -U promptinit
promptinit
prompt walters

. ~/dotfiles/scripts/zshrc.sh
MYSHELL=$(ps -p $$ -ocomm= 2>/dev/null)
NEWLINE=$'\n'
PROMPT="${NEWLINE}%{$fg_bold[yellow]%}%~\$(git_super_status) %(?.%{$fg[green]%}.%{$fg[red]%})%#%{$reset_color%} "
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
bindkey "^[[A" up-line-or-beginning-search
bindkey "^[[B" down-line-or-beginning-search

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
