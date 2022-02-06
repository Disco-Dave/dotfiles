#!/bin/zsh

set -e

source "$_DOTFILES_HOME/zsh/zshenv"

sudo pacman -S --needed --noconfirm armagetronad

mkdir -p ~/.armagetronad/var
rm ~/.armagetronad/var/autoexec.cfg
ln -sf "$_DOTFILES_HOME/armagetronad/var/autoexec.cfg" ~/.armagetronad/var/autoexec.cfg
