#!/bin/bash

set -e

git pull
sudo pacman -Syu --noconfirm 
npm -g update neovim
stack upgrade
(cd $HOME/.local/share/zsh/plugins/zsh-git-prompt; git pull; stack install)
nvim +PlugUpdate +qall
