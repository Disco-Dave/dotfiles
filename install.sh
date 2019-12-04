#!/bin/bash

###################################
#               XDG               #
###################################

# Ensure that the XDG env variables are set
mkdir -p $HOME/.config $HOME/.cache $HOME/.local/share
sudo bash -c "echo XDG_CONFIG_HOME=\$HOME/.config >> /etc/environment"
sudo bash -c "echo XDG_CACHE_HOME=\$HOME/.cache >> /etc/environment"
sudo bash -c "echo XDG_DATA_HOME=\$HOME/.local/share >> /etc/environment"





###################################
#               SHELL             #
###################################

# Install zsh
sudo pacman -S --noconfirm --needed zsh zsh-syntax-highlighting zsh-autosuggestions zsh-history-substring-search

# Set ZSH dot directory
sudo bash -c "echo 'export ZDOTDIR=\$HOME/.config/zsh' >> /etc/zsh/zshenv"

# Link ZSH files
ln -sf `pwd`/zsh $HOME/.config/zsh

# Install oh-my-zsh
export ZSH=$HOME/.config/oh-my-zsh
sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
