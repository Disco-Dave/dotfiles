#!/bin/bash

# Exit this script if any command returns a non-zero exit status
set -e


# XDG Directories
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"

mkdir -p "$XDG_CONFIG_HOME" "$XDG_CACHE_HOME" "$XDG_DATA_HOME"


# Install all required packages via pacman
sudo pacman -Syu --noconfirm
sudo pacman -S --needed --noconfirm \
    gcc git git gmp make neovim nodejs npm \
    openssh xclip zlib zsh zsh \
    zsh-autosuggestions zsh-completions \
    zsh-history-substring-search \
    zsh-syntax-highlighting 


if [[ -d "$XDG_CONFIG_HOME/dotfiles" ]]; then
    cd "$XDG_CONFIG_HOME/dotfiles"
    git pull
else
    git clone https://github.com/Disco-Dave/dotfiles.git \
        "$XDG_CONFIG_HOME/dotfiles"
    cd "$XDG_CONFIG_HOME/dotfiles"
fi


# Setup npm
if [[ ! -f "$XDG_CONFIG_HOME/npm/npmrc" ]]; then
    mkdir -p "$XDG_CONFIG_HOME/npm"
    echo "prefix=$XDG_DATA_HOME/npm" >> "$XDG_CONFIG_HOME/npm/npmrc"
    echo "cache=$XDG_CACHE_HOME/npm" >> "$XDG_CONFIG_HOME/npm/npmrc"
    echo "tmp=\$XDG_RUNTIME_DIR/npm" >> "$XDG_CONFIG_HOME/npm/npmrc"
    echo "init-module=$XDG_CONFIG_HOME/npm/config/npm-init.js" >> "$XDG_CONFIG_HOME/npm/npmrc"
fi
export NPM_CONFIG_USERCONFIG="$HOME/.config/npm/npmrc"


# Install haskell stack
# https://docs.haskellstack.org/en/stable/README/
export STACK_ROOT="$XDG_DATA_HOME/stack"
if [[ ! -f "/usr/local/bin/stack" ]]; then
    curl -sSL https://get.haskellstack.org/ | sh
    stack setup
fi


# Link up git config
ln -sf "$(pwd)/git" "$XDG_CONFIG_HOME/git"


# Setup zsh
echo "export ZDOTDIR=$XDG_CONFIG_HOME/zsh" | sudo tee /etc/zsh/zshenv > /dev/null
mkdir -p "$XDG_CONFIG_HOME/zsh"
mkdir -p "$XDG_DATA_HOME/zsh/plugins"
mkdir -p "$XDG_CACHE_HOME/zsh"

if [[ ! -d "$XDG_DATA_HOME/zsh/plugins/zsh-git-prompt" ]]; then
    git clone https://github.com/olivierverdier/zsh-git-prompt.git \
        $XDG_DATA_HOME/zsh/plugins/zsh-git-prompt
    (cd $XDG_DATA_HOME/zsh/plugins/zsh-git-prompt; stack install)
fi

ln -sf "$(pwd)/zsh/zshenv" "$XDG_CONFIG_HOME/zsh/.zshenv"
ln -sf "$(pwd)/zsh/zshrc" "$XDG_CONFIG_HOME/zsh/.zshrc"

if [ "$SHELL" != "/bin/zsh" ]; then
    chsh -s "/bin/zsh" "$USER"

    rm -rf "$HOME/.bash_logout"
    rm -rf "$HOME/.bash_profile"
    rm -rf "$HOME/.bash_history"
    rm -rf "$HOME/.bashrc"
fi


# Setup neovim
npm install -g neovim

ln -sf "$(pwd)/nvim" "$XDG_CONFIG_HOME/nvim"
mkdir -p "$XDG_DATA_HOME/nvim/backup"
mkdir -p "$XDG_DATA_HOME/nvim/swap"
mkdir -p "$XDG_DATA_HOME/nvim/undo"
mkdir -p "$XDG_DATA_HOME/nvim/language-servers"

if [[ ! -f "$XDG_DATA_HOME/nvim/site/autoload/plug.vim" ]]; then
    sh -c 'curl -fLo "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/autoload/plug.vim --create-dirs \
           https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
    nvim +PlugInstall +qall
fi
