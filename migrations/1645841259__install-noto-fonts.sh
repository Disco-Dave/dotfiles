#!/bin/zsh

set -e

source "$_DOTFILES_HOME/zsh/zshenv"

sudo pacman -S --needed --noconfirm noto-fonts noto-fonts-cjk noto-fonts-emoji noto-fonts-extra
