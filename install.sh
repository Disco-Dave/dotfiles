#!/bin/bash

###################################
#               XDG               #
###################################

# Ensure that the XDG env variables are set
mkdir -p $HOME/.config $HOME/.cache $HOME/.local/share
xdg_vars=("export XDG_CONFIG_HOME=\$HOME/.config" 
          "export XDG_CACHE_HOME=\$HOME/.cache" 
          "export XDG_DATA_HOME=\$HOME/.localshare")
if [ ! -f /etc/zsh/zshenv ]; then
    sudo touch /etc/zsh/zshenv
fi
for var in $xdg_vars; do
    if grep -q $var /etc/zsh/zshenv; then
        echo $var | sudo tee -a /etc/zsh/zshenv > /dev/null
    fi
done





###################################
#               SHELL             #
###################################

# Install zsh
sudo pacman -S --noconfirm --needed zsh

# Set ZSH dot directory
$zdotdir="export ZDOTDIR=\$HOME/.config/zsh"
if grep -q $zdotdir; then
    echo $zdotdir | sudo tee -a /etc/zsh/zshenv > /dev/null
fi

# Ensure history file is made
mkdir -p $HOME/.local/share/zsh
mkdir -p $HOME/.cache/zsh
touch $HOME/.local/share/zsh/history

# Install oh-my-zsh
export ZSH=$HOME/.config/oh-my-zsh
if [ ! -d $ZSH ]; then
    sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)" "" --unattended
    git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-$ZSH/custom}/plugins/zsh-syntax-highlighting
    git clone git://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-$ZSH/custom}/plugins/zsh-autosuggestions
    git clone https://github.com/zsh-users/zsh-history-substring-search  ${ZSH_CUSTOM:-$ZSH/custom}/plugins/zsh-history-substring-search
fi

# Link ZSH files
ln -sfn `pwd`/zsh $HOME/.config/zsh

# Add zsh theme
cp $HOME/.config/zsh/clean-newline.zsh-theme $HOME/.config/oh-my-zsh/themes/clean-newline.zsh-theme





###################################
#               VIM               #
###################################

# Install vim and other dependencies
sudo pacman -S --noconfirm --needed gvim xclip nodejs npm ruby python python2 stack

# Create vim directories
mkdir -p $HOME/.local/share/vim/{undo,swap,backup}

# Install vim-plug
if [ ! -f ~/.local/share/vim/site/autoload/plug.vim ]; then
    curl -fLo ~/.local/share/vim/site/autoload/plug.vim --create-dirs \
        https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
fi

# Link Vim files
ln -sfn `pwd`/vim $HOME/.config/vim
