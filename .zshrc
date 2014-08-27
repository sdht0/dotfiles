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

MYSHELL=$(ps -hp $$ | awk '{print $5}' | sed 's/.*[^a-z]\([a-z]*\)/\1/')
[[ $UID -eq 0 ]] && color=red || color=green
NEWLINE=$'\n'
PROMPT="${NEWLINE}%{$fg_bold[blue]%}[${MYSHELL}]%{$reset_color%} [%{$fg_bold[${color}]%}%n%{$reset_color%}@%{$fg_bold[magenta]%}%M%{$reset_color%}] %{$fg_bold[yellow]%}(%D{%a, %b %d, %I:%M:%S %P}): %{$fg_bold[magenta]%}%~${NEWLINE}%(?.%{$fg[green]%}✔.%{$fg[red]%}✘)%{$reset_color%} %# "
RPROMPT=""

typeset -g -A key
#bindkey '\e[3~' delete-char
bindkey '\e[1~' beginning-of-line
bindkey '\e[4~' end-of-line
#bindkey '\e[2~' overwrite-mode
bindkey '^?' backward-delete-char
bindkey '^[[1~' beginning-of-line
bindkey '^[[3~' delete-char
bindkey '^[[4~' end-of-line
bindkey '^[[A' up-line-or-search
bindkey '^[[D' backward-char
bindkey '^[[B' down-line-or-search
bindkey '^[[C' forward-char
# for rxvt
bindkey "\e[8~" end-of-line
bindkey "\e[7~" beginning-of-line
bindkey "^[[H" beginning-of-line
bindkey "^[[F" end-of-line
bindkey '^U' backward-kill-line
bindkey '^Y' yank

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
