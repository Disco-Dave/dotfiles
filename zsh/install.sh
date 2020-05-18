#!/bin/bash

# Install zsh packages
sudo pacman -S --needed zsh zsh-autosuggestions zsh-completions \
    zsh-history-substring-search zsh-syntax-highlighting

# User XDG Base Directory for ZSH
# https://wiki.archlinux.org/index.php/XDG_Base_Directory#Support
echo "export ZDOTDIR=$HOME/.config/zsh" | sudo tee -a /etc/zsh/zshenv > /dev/null
mkdir -p "$HOME/.config/zsh"
mkdir -p "$HOME/.local/share/zsh"
mkdir -p "$HOME/.cache/zsh"

# Link files to correct directories
echo "`pwd`/zshenv" "$HOME/.config/zsh/.zshenv"
echo "`pwd`/zshrc" "$HOME/.config/zsh/.zshrc"

# Set zsh as the default shell
chsh -s /bin/zsh
