. ~/.bashrc

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

export TERM=screen-256color

if [[ $- = *i* ]];then

powerline=~/.dotfiles/powerlevel10k/powerlevel10k.zsh-theme
if [[ -r "$powerline" ]] && [[ ! $TTY = *tty* ]];then
    #POWERLEVEL9K_MODE='awesome-fontconfig'

    POWERLEVEL9K_PROMPT_ON_NEWLINE=true
    POWERLEVEL9K_PROMPT_ADD_NEWLINE=true

    POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(context dir dir_writable status)
    POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(vcs time)

    POWERLEVEL9K_TIME_BACKGROUND='blue'

    POWERLEVEL9K_CONTEXT_DEFAULT_BACKGROUND='black'
    POWERLEVEL9K_CONTEXT_DEFAULT_FOREGROUND='cyan'

    POWERLEVEL9K_VCS_DEFAULT_BACKGROUND='blue'

    POWERLEVEL9K_DIR_DEFAULT_BACKGROUND='cyan'
    POWERLEVEL9K_DIR_HOME_BACKGROUND='cyan'
    POWERLEVEL9K_DIR_HOME_SUBFOLDER_BACKGROUND='cyan'

    POWERLEVEL9K_MULTILINE_FIRST_PROMPT_PREFIX="%F{cyan}\u256D\u2500%f"
    POWERLEVEL9K_MULTILINE_LAST_PROMPT_PREFIX="%F{cyan}╰❱❱❱%f "

    #POWERLEVEL9K_MULTILINE_FIRST_PROMPT_PREFIX="" #"╰╭"
    #POWERLEVEL9K_MULTILINE_LAST_PROMPT_PREFIX="❱❱❱ "
    #POWERLEVEL9K_MULTILINE_SECOD_PROMPT_PREFIX="%(?.%{$fg[green]%}.%{$fg[red]%})%#%{$reset_color%} "
    
    source "$powerline"
    unset LC_CTYPE
else
    MYSHELL=$(ps -p $$ -ocomm= 2>/dev/null)
    [[ $UID -eq 0 ]] && color=red || color=magenta
    NEWLINE=$'\n'
    PROMPT="${NEWLINE}[%{$fg_bold[blue]%}${MYSHELL}%{$reset_color%}:%{$fg_bold[${color}]%}%n@%M%{$reset_color%}][%D{%H:%M:%S}] %{$fg_bold[green]%}%~${NEWLINE}%(?.%{$fg[green]%}.%{$fg[red]%})%#%{$reset_color%} "
    RPROMPT=""
fi

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

better_history=~/.dotfiles/zsh-peco-history/zsh-peco-history.zsh
if [[ -r "$better_history" ]];then
    source "$better_history"
fi

interactive_cd=~/.dotfiles/zsh-interactive-cd/zsh-interactive-cd.plugin.zsh
if [[ -r "$interactive_cd" ]];then
    export FZF_DEFAULT_OPTS="--cycle"
    source "$interactive_cd"
fi

autosuggestions=~/.dotfiles/zsh-autosuggestions/zsh-autosuggestions.zsh
if [[ -r "$autosuggestions" ]];then
    source "$autosuggestions"
fi

syntax_highlighting=~/.dotfiles/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
if [[ -r "$syntax_highlighting" ]];then
    source "$syntax_highlighting"
fi

fi

