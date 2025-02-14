HISTFILE=${DOTFILES}.safe/bash_history
HISTSIZE=100000
SAVEHIST=$HISTSIZE

autoload -U colors && colors
autoload -U compinit
compinit -i
zstyle ':completion:*' rehash yes
autoload -U promptinit
promptinit
prompt walters

export TERM=screen-256color
unset LANGUAGE

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
    interactivecomments \
    autocd \
    autopushd \
    pushdignoredups \
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

if [[ $- = *i* ]];then

    if [[ -f "$DOTFILES/profile.server" ]];then
        local config_file=$DOTFILES/prompt/server.starship.toml
    else
        local config_file=$DOTFILES/prompt/starship.toml
    fi

    if command -v starship &>/dev/null && [[ -f "$config_file" ]] ;then
        export STARSHIP_CONFIG="$config_file"
        eval "$(starship init zsh)"
    else
        [[ $UID -eq 0 ]] && color=red || color=magenta
        local NEWLINE=$'\n'
        PROMPT="${NEWLINE}[%{$fg_bold[blue]%}${MYSHELL}%{$reset_color%}:%{$fg_bold[${color}]%}%n@%M%{$reset_color%}][%D{%H:%M:%S}] %{$fg_bold[green]%}%~${NEWLINE}%(?.%{$fg[green]%}.%{$fg[red]%})%#%{$reset_color%} "
        RPROMPT=""
    fi

    local better_history=$DOTFILES/modules/zsh-peco-history/zsh-peco-history.zsh
    if [[ -r "$better_history" ]];then
        source "$better_history"
    fi

    local interactive_cd=$DOTFILES/modules/zsh-interactive-cd/zsh-interactive-cd.plugin.zsh
    if [[ -r "$interactive_cd" ]] && which fzf &>/dev/null ;then
        export FZF_DEFAULT_OPTS="--cycle"
        source "$interactive_cd"
    fi

    local autosuggestions=$DOTFILES/modules/zsh-autosuggestions/zsh-autosuggestions.zsh
    if [[ -r "$autosuggestions" ]];then
        source "$autosuggestions"
    fi

    local syntax_highlighting=$DOTFILES/modules/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
    if [[ -r "$syntax_highlighting" ]];then
        source "$syntax_highlighting"
    fi

    if [[ -f /opt/homebrew/bin/brew ]];then
        eval "$(/opt/homebrew/bin/brew shellenv)"
    fi

fi
