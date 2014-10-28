#!/bin/bash

scp -r /home/sdh/dotfiles/{.tmux.conf,.bashrc,.gitconfig,.vim*,.zshrc} root@192.168.33.10:~
scp -r /home/sdh/dotfiles/{.tmux.conf,.bashrc,.gitconfig,.vim*,.zshrc} root@192.168.33.11:~
scp -r /home/sdh/dotfiles/{getips,getuptime}.sh root@192.168.33.10:~/dotfiles/
scp -r /home/sdh/dotfiles/{getips,getuptime}.sh root@192.168.33.11:~/dotfiles/
