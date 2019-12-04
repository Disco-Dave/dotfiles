#!/bin/bash

###################################
#               XDG               #
###################################

# Ensure that the XDG env variables are set
mkdir -p $HOME/.config $HOME/.cache $HOME/.local/share
echo XDG_CONFIG_HOME=\$HOME/.config | sudo tee -a /etc/environment > /dev/null
echo XDG_CACHE_HOME=\$HOME/.cache | sudo tee -a /etc/environment > /dev/null
echo XDG_DATA_HOME=\$HOME/.local/share | sudo tee -a /etc/environment > /dev/null





###################################
#               SHELL             #
###################################

# Install zsh
sudo pacman -S --noconfirm --needed zsh zsh-syntax-highlighting zsh-autosuggestions zsh-history-substring-search

# Set ZSH dot directory
echo "export ZDOTDIR=\$HOME/.config/zsh" | sudo tee -a /etc/zsh/zshenv

# Link ZSH files
ln -sf `pwd`/zsh $HOME/.config/zsh

# Install oh-my-zsh
export ZSH=$HOME/.config/oh-my-zsh
sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
