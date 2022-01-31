#!/bin/zsh

set -e

source "$_DOTFILES_HOME/zsh/zshenv"

cabal install --overwrite-policy=always ShellCheck
sudo pacman -S --needed --noconfirm shfmt
