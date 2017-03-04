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

export TERM=xterm-256color

if [[ -r ~/powerlevel9k/powerlevel9k.zsh-theme ]];then
    source  ~/powerlevel9k/powerlevel9k.zsh-theme
    POWERLEVEL9K_MODE='awesome-patched'

    POWERLEVEL9K_PROMPT_ON_NEWLINE=true
    POWERLEVEL9K_PROMPT_ADD_NEWLINE=true

    POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(context vcs dir)
    POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(status time)

    POWERLEVEL9K_TIME_BACKGROUND='green'

    #POWERLEVEL9K_STATUS_OK_BACKGROUND='white'
    #POWERLEVEL9K_STATUS_ERROR_BACKGROUND='white'

    POWERLEVEL9K_CONTEXT_DEFAULT_BACKGROUND='blue'
    POWERLEVEL9K_CONTEXT_DEFAULT_FOREGROUND='black'

    POWERLEVEL9K_DIR_DEFAULT_BACKGROUND='226'
    POWERLEVEL9K_DIR_HOME_BACKGROUND='226'
    POWERLEVEL9K_DIR_HOME_SUBFOLDER_BACKGROUND='226'

    POWERLEVEL9K_MULTILINE_FIRST_PROMPT_PREFIX=""
    POWERLEVEL9K_MULTILINE_SECOND_PROMPT_PREFIX="%(?.%{$fg[green]%}.%{$fg[red]%})%#%{$reset_color%} "
else
    source ~/zsh-git-prompt/zshrc.sh 2>/dev/null
    GIT_PROMPT_EXECUTABLE="haskell"
    MYSHELL=$(ps -p $$ -ocomm= 2>/dev/null)
    [[ $UID -eq 0 ]] && color=red || color=green
    NEWLINE=$'\n'
    PROMPT="${NEWLINE}[%{$fg_bold[blue]%}${MYSHELL}%{$reset_color%}:%{$fg_bold[${color}]%}%n@%M%{$reset_color%}][%D{%H:%M:%S}] %{$fg_bold[yellow]%}%~ \$(git_super_status 2>/dev/null)${NEWLINE}%(?.%{$fg[green]%}.%{$fg[red]%})%#%{$reset_color%} "
    RPROMPT=""
fi

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
