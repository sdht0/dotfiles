autoload -Uz colors && colors
autoload -Uz compinit && compinit -i && zstyle ':completion:*' rehash yes
autoload -Uz promptinit && promptinit && prompt walters

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

setopt nobeep                   # Disable the terminal beep sound
setopt notify                   # Notify when background jobs complete
setopt nobgnice                 # Do not automatically lower the priority of background jobs
setopt interactivecomments      # Allow comments anywhere on interactive command lines

setopt autocd                   # Just type directory name to cd
setopt autopushd                # Push the old directory onto the stack
setopt pushdignoredups          # Don't store duplicates in the stack
setopt pushdtohome              # pushd -> push & cd $HOME

setopt nochaselinks             # If you cd into a symbolic link, Zsh will NOT change to the real physical directory
setopt histverify               # When using history expansion (like !!, !$, etc.), don't execute immediately

setopt sharehistory             # Share command history between all Zsh instances.
setopt histappend               # Appends new history entries to the history file rather than overwriting it
setopt hist_reduce_blanks       # Remove unnecessary blanks
setopt hist_ignore_space        # Don't save commands to history if they start with a space
setopt hist_ignore_all_dups     # Don't record duplicates in history
setopt hist_save_no_dups        # Don't write duplicate entries

setopt braceccl                 # Enables brace character class lists
setopt dotglob                  # Makes globs (like *) match files starting with a dot (.)
setopt extendedglob             # Enables advanced globbing features
setopt numericglobsort          # Makes numeric filenames sort numerically rather than alphabetically
setopt nolisttypes              # Disables displaying the type of files (like @ for symlinks) in completion listings
setopt promptsubst              # Enables command substitution in prompts
setopt completealiases          # Makes aliases have their own completion definitions

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

    # interactive history search
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
        export ZSH_AUTOSUGGEST_STRATEGY=(history completion)
        source "$autosuggestions"
    fi

    local syntax_highlighting=$DOTFILES/modules/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
    if [[ -r "$syntax_highlighting" ]];then
        source "$syntax_highlighting"
    fi

    if [[ -f /opt/homebrew/bin/brew ]];then
        eval "$(/opt/homebrew/bin/brew shellenv)"
    fi

    if [[ -f /opt/homebrew/opt/rustup/bin ]];then
        path+="/opt/homebrew/opt/rustup/bin" # zsh-style path appends
    fi

fi
