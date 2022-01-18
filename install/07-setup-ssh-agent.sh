#!/usr/bin/env zsh

echo "-- Setup ssh agent --"

source "$_DOTFILES_HOME/zsh/zshenv"
set -e

mkdir -p "$HOME/.ssh"

if [[ ! -f "$HOME/.ssh/config" ]]; then
  echo "AddKeysToAgent yes" >> "$HOME/.ssh/config"
fi

mkdir -p "$XDG_CONFIG_HOME/systemd/user"

{
  echo "[Unit]"
  echo "Description=SSH key agent"
  echo ""
  echo "[Service]"
  echo "Type=simple"
  echo "Environment=SSH_AUTH_SOCK=%t/ssh-agent.socket"
  echo "# DISPLAY required for ssh-askpass to work"
  echo "Environment=DISPLAY=:0"
  echo "ExecStart=/usr/bin/ssh-agent -D -a \$SSH_AUTH_SOCK"
  echo ""
  echo "[Install]"
  echo "WantedBy=default.target"
} >> "$XDG_CONFIG_HOME/systemd/user/ssh-agent.service"

if [ "$ENVIRONMENT" != "sandbox" ]; then
  systemctl --user enable --now ssh-agent.service
fi
