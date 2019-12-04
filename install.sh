#!/bin/bash

###################################
#       Setup shell and XDG       #
###################################

# Install zsh and xdg packages
pacman -S --noconfirm --needed zsh xdg-utils xdg-user-dirs

# Ensure that the XDG env variables are set
mkdir -p $HOME/.config $HOME/.cache $HOME/.local/share
echo XDG_CONFIG_HOME=\$HOME/.config >> /etc/environment
echo XDG_CACHE_HOME=\$HOME/.cache >> /etc/environment
echo XDG_DATA_HOME=\$HOME/.local/share >> /etc/environment

# Set ZSH dot directory
echo "export ZDOTDIR=\$HOME/.config/zsh" >> /etc/zsh/zshenv

# Link ZSH files
ln -sf `pwd`/zsh $HOME/.config/zsh
