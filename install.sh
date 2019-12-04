#!/bin/bash

###################################
#               XDG               #
###################################

# Ensure that the XDG env variables are set
mkdir -p $HOME/.config $HOME/.cache $HOME/.local/share
echo "export XDG_CONFIG_HOME=\$HOME/.config" | sudo tee -a /etc/zsh/zshenv > /dev/null
echo "export XDG_CACHE_HOME=\$HOME/.cache" | sudo tee -a /etc/zsh/zshenv > /dev/null
echo "export XDG_DATA_HOME=\$HOME/.local/share" | sudo tee -a /etc/zsh/zshenv > /dev/null





###################################
#               SHELL             #
###################################

# Install zsh
sudo pacman -S --noconfirm --needed zsh

# Set ZSH dot directory
echo "export ZDOTDIR=\$HOME/.config/zsh" | sudo tee -a /etc/zsh/zshenv > /dev/null

# Ensure history file is made
mkdir -p $HOME/.local/share/zsh
mkdir -p $HOME/.cache/zsh
touch $HOME/.local/share/zsh/history

# Install oh-my-zsh
export ZSH=$HOME/.config/oh-my-zsh
sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)" "" --unattended
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-$ZSH/custom}/plugins/zsh-syntax-highlighting
git clone git://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-$ZSH/custom}/plugins/zsh-autosuggestions
git clone https://github.com/zsh-users/zsh-history-substring-search  ${ZSH_CUSTOM:-$ZSH/custom}/plugins/zsh-history-substring-search

# Link ZSH files
ln -sf `pwd`/zsh $HOME/.config/zsh

# Add zsh theme
cp $HOME/.config/zsh/clean-newline.zsh-theme $HOME/.config/oh-my-zsh/themes/clean-newline.zsh-theme





###################################
#               VIM               #
###################################

# Install vim and other dependencies
sudo pacman -S --noconfirm --needed gvim xclip node ruby python python2 stack

# Create vim directories
mkdir -p "$XDG_DATA_HOME"/vim/{undo,swap,backup}

# Link Vim files
ln -sf `pwd`/zsh $HOME/.config/vim
