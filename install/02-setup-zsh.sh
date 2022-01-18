#!/usr/bin/env zsh

echo "-- Setup ZSH --"

source "$_DOTFILES_HOME/zsh/zshenv"
set -e

sudo pacman -S --noconfirm --needed zsh

# Tell zsh to look for dotfiles in "$XDG_CONFIG_HOME/zsh"
echo "export ZDOTDIR=$XDG_CONFIG_HOME/zsh" | sudo tee /etc/zsh/zshenv > /dev/null

mkdir -p "$XDG_CONFIG_HOME/zsh"
mkdir -p "$XDG_DATA_HOME/zsh"
mkdir -p "$XDG_CACHE_HOME/zsh"

# Link configuration files
ln -sfn "$_DOTFILES_HOME/zsh/zprofile" "$XDG_CONFIG_HOME/zsh/.zprofile"
ln -sfn "$_DOTFILES_HOME/zsh/zshenv" "$XDG_CONFIG_HOME/zsh/.zshenv"
ln -sfn "$_DOTFILES_HOME/zsh/zshrc" "$XDG_CONFIG_HOME/zsh/.zshrc"

# Install zplug
if [ ! -d "ZPLUG_HOME" ]; then
  curl -sL --proto-redir -all,https https://raw.githubusercontent.com/zplug/installer/master/installer.zsh | zsh
fi

# Switch our shell to zsh
if [ "$SHELL" != "/bin/zsh" ]; then
  sudo chsh -s "/bin/zsh" "$(whoami)"

  rm -rf "$HOME/.bash_logout"
  rm -rf "$HOME/.bash_profile"
  rm -rf "$HOME/.bash_history"
  rm -rf "$HOME/.bashrc"
fi
