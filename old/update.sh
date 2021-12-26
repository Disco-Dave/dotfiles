#!/bin/bash

set -e

git pull
sudo pacman -Syu --noconfirm 
yay --aur -Syu --noconfirm
npm -g update neovim
stack upgrade
rustup update
(cd $HOME/.local/share/zsh/plugins/zsh-git-prompt; git pull; stack install)
nvim +PlugUpdate +qall
(cd $HOME/.config/alacritty/gen-alacritty; stack install; gen-alacritty)
