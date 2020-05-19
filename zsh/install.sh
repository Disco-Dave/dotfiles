#!/bin/bash

# Install zsh packages
sudo pacman -S --needed --noconfirm zsh zsh-autosuggestions zsh-completions \
    zsh-history-substring-search zsh-syntax-highlighting ruby openssh

# Use XDG Base Directory for ZSH
# https://wiki.archlinux.org/index.php/XDG_Base_Directory#Support
echo "export ZDOTDIR=$HOME/.config/zsh" | sudo tee /etc/zsh/zshenv > /dev/null
mkdir -p "$HOME/.config/zsh"
mkdir -p "$HOME/.local/share/zsh"
mkdir -p "$HOME/.cache/zsh"

# Link files to correct directories
ln -sf "`pwd`/zshenv" "$HOME/.config/zsh/.zshenv"
ln -sf "`pwd`/zshrc" "$HOME/.config/zsh/.zshrc"

# Set zsh as the default shell
if [ "$SHELL" != "/bin/zsh" ]; then
    chsh -s /bin/zsh

    # Remove bash files
    rm -rf $HOME/.bash_logout
    rm -rf $HOME/.bash_profile
    rm -rf $HOME/.bashrc

    zsh
fi
