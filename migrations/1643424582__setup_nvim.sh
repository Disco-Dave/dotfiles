#!/bin/zsh

set -e

source "$_DOTFILES_HOME/zsh/zshenv"

sudo pacman -S --noconfirm --needed neovim

npm install -g neovim

ln -sfn "$_DOTFILES_HOME/nvim" "$XDG_CONFIG_HOME/nvim"
mkdir -p "$XDG_DATA_HOME/nvim/backup"
mkdir -p "$XDG_DATA_HOME/nvim/swap"
mkdir -p "$XDG_DATA_HOME/nvim/undo"

if [[ ! -f "$XDG_DATA_HOME/nvim/site/autoload/plug.vim" ]]; then
  sh -c 'curl -fLo "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/autoload/plug.vim --create-dirs \
           https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  nvim +PlugInstall +qall
  nvim +CocUpdateSync +qall
fi
