#!/bin/bash

cp -rv /home/sdh/dotfiles/{.tmux.conf,.bashrc,.gitconfig,.vim*,.zshrc} /home/sdh/directi/sources/user-files/siddhartha.s/
cp -rv /home/sdh/dotfiles/scripts/{getips,getuptime}.sh /home/sdh/directi/sources/user-files/siddhartha.s/dotfiles/

cp -rv /home/sdh/dotfiles/{.tmux.conf,.bashrc,.gitconfig,.zshrc} /home/sdh/directi/sources/docker-puppet/base
cp -rv /home/sdh/dotfiles/scripts/{getips,getuptime}.sh /home/sdh/directi/sources/docker-puppet/base
