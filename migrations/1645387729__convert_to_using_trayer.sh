#!/bin/zsh

set -e

source "$_DOTFILES_HOME/zsh/zshenv"

sudo pacman -S --needed --noconfirm trayer
sudo pacman -Rs --noconfirm stalonetray

rm -rf "$XDG_CONFIG_HOME/stalonetray"

cd "$_DOTFILES_HOME/xmonad"
cabal install --overwrite-policy=always
xmonad --recompile
