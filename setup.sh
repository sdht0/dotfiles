#!/bin/bash

dotfiles=~/.dotfiles
bkdir=~/.backup_dotfiles/"$(date +"%Y%m%d-%H%M%S")"

link_file() {
    [[ -z "$1" ]] && return -1
    dest="${2:-.$1}"
    echo "Setting up $dest"
    [[ -e ~/"$dest" ]] && echo "Backing up $dest" && mkdir -p ${bkdir} && mv ~/"$dest" ${bkdir}/"$dest".bk
    ln -s "$dotfiles/$1" ~/$dest
}

echo "Setting up .bashrc"
[[ -r ~/.bashrc ]] && echo "Backing up .bashrc" && mkdir -p ${bkdir} && mv ~/.bashrc ${bkdir}/.bashrc.bk
echo -e ". $dotfiles/bashrc" > ~/.bashrc

echo "Setting up .zshrc"
[[ -r ~/.zshrc ]] && echo "Backing up .zshrc" && mkdir -p ${bkdir} && mv ~/.zshrc ${bkdir}/.zshrc.bk
echo -e ". ~/.bashrc\n. $dotfiles/zshrc" > ~/.zshrc

# echo "Setting up .emacs"
# [[ -r ~/.emacs.d/init.el ]] && echo "Backing up init.el" mkdir -p ${bkdir} && && mv ~/.emacs.d/init.el ${bkdir}/.emacs.d/init.el.bk
# link_file "emacs.init.el" ".emacs.d/init.el"

link_file "tmux.conf"
link_file "gitconfig"
link_file "vimrc"

