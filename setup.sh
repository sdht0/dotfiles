#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
[[ -z "$DIR" ]] && echo "Could not find script directory" && exit -1

link_file() {
    [[ -z "$1" ]] && return -1
    echo "Setting up $1"
    [[ -r ~/"$1" ]] && echo "Backing up $1" && mv ~/"$1" ~/"$1".bk
    rm -f ~/"$1" && ln -s "$DIR/$1" ~
}

echo "Setting up .bashrc"
[[ -r ~/.bashrc ]] && echo "Backing up .bashrc" && mv ~/.bashrc ~/.bashrc.bk
echo -e ". $DIR/.bashrc" > ~/.bashrc

echo "Setting up .zshrc"
[[ -r ~/.zshrc ]] && echo "Backing up .zshrc" && mv ~/.zshrc ~/.zshrc.bk
echo -e ". $DIR/.bashrc\n. $DIR/.zshrc" > ~/.zshrc

link_file ".tmux.conf"
link_file ".gitconfig"
link_file ".vimrc"
