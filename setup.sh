#!/bin/bash

dotfiles=~/.dotfiles
bkdir=~/.cache/dotfiles-"$(date +"%Y%m%d-%H%M%S")"

link_file() {
    [[ -z "$1" ]] && return -1
    dest="${2:-.$1}"
    echo "Setting up $dest"
    [[ -e ~/"$dest" ]] && echo "Backing up $dest" && mkdir -p ${bkdir} && mv ~/"$dest" ${bkdir}/"$dest"
    ln -s "$dotfiles/$1" ~/$dest
}

echo "Setting up .bashrc"
[[ -r ~/.bashrc ]] && echo "Backing up .bashrc" && mkdir -p ${bkdir} && mv ~/.bashrc ${bkdir}/.bashrc
echo -e ". $dotfiles/bashrc\n[[ -f $dotfiles.safe/bashrc ]] && . $dotfiles.safe/bashrc" > ~/.bashrc

echo "Setting up .zshrc"
[[ -r ~/.zshrc ]] && echo "Backing up .zshrc" && mkdir -p ${bkdir} && mv ~/.zshrc ${bkdir}/.zshrc
echo -e ". ~/.bashrc\n. $dotfiles/zshrc\n[[ -f $dotfiles.safe/bashrc ]] && . $dotfiles.safe/bashrc" > ~/.zshrc

# echo "Setting up .emacs"
# [[ -r ~/.emacs.d/init.el ]] && echo "Backing up init.el" mkdir -p ${bkdir} && && mv ~/.emacs.d/init.el ${bkdir}/.emacs.d/init.el.bk
# link_file "emacs.init.el" ".emacs.d/init.el"

link_file "tmux.conf"
link_file "gitconfig"
link_file "vimrc"
# link_file "xinitrc"
