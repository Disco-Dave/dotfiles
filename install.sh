#!/bin/bash

###################################
#       Setup shell and XDG       #
###################################

# Install zsh and xdg packages
sudo pacman -S --noconfirm --needed zsh xdg-utils xdg-user-dirs

# Set ZSH as default shell
chsh -s /bin/zsh

# Ensure that the XDG env variables are set
mkdir -p $HOME/.config $HOME/.cache $HOME/.local/share
sudo echo XDG_CONFIG_HOME=\$HOME/.config >> /etc/environment
sudo echo XDG_CACHE_HOME=\$HOME/.cache >> /etc/environment
sudo echo XDG_DATA_HOME=\$HOME/.local/share >> /etc/environment

# Set ZSH dot directory
sudo echo "export ZDOTDIR=\$HOME/.config/zsh" >> /etc/zsh/zshenv

# Link ZSH files
ln -sf `pwd`/zsh $HOME/.config/zsh
