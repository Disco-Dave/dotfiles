#!/bin/zsh

set -e

source "$_DOTFILES_HOME/zsh/zshenv"

if [[ ! -e ~/Documents/wiki ]]; then
  git clone git@gitlab.com:disco-dave/wiki ~/Documents/wiki
fi

rm -rf ~/.local/share/personal-wiki
rm -rf ~/.local/share/work-wiki
