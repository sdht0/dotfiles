#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
[[ -z "$DIR" ]] && echo "Could not find script directory" && exit -1

link_file() {
    [[ -z "$1" ]] && return -1
    dest="${2:-.$1}"
    echo "Setting up $dest"
    [[ -e ~/"$dest" ]] && echo "Backing up $dest" && mv ~/"$dest" ~/"$dest".bk
    ln -s "$DIR/$1" ~/$dest
}

echo "Setting up .bashrc"
[[ -r ~/.bashrc ]] && echo "Backing up .bashrc" && mv ~/.bashrc ~/.bashrc.bk
echo -e ". $DIR/bashrc" > ~/.bashrc

echo "Setting up .zshrc"
[[ -r ~/.zshrc ]] && echo "Backing up .zshrc" && mv ~/.zshrc ~/.zshrc.bk
echo -e ". $DIR/bashrc\n. $DIR/zshrc" > ~/.zshrc

echo "Setting up .emacs"
[[ -r ~/.emacs.d/init.el ]] && echo "Backing up init.el" && mv ~/.emacs.d/init.el ~/.emacs.d/init.el.bk
link_file "emacs.init.el" ".emacs.d/init.el"

link_file "tmux.conf"
link_file "gitconfig"
link_file "vimrc"
