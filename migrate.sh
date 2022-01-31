#!/bin/bash

set -e

dotfiles_home="$(dirname "$(realpath "$0")")"
export _DOTFILES_HOME="$dotfiles_home"

local_hostname="$(hostnamectl hostname)"
export _HOSTNAME="$local_hostname"

function print_usage() {
  cat << EOF
Manage or execute migration scripts for my dotfiles
Usage: $0 COMMAND
Commands:
  help                  Print this help message
  run                   Execute migration scripts that have not been ran yet
  new MIGRATION_NAME    Generate a new migration in the ./migrations directory
EOF
}

function run_scripts() {
  migration_data="$HOME/.local/share/dotfiles/scripts_ran"
  mkdir -p "$(dirname "$migration_data")"

  if [[ ! -f "$migration_data" ]]; then
    touch "$migration_data"
  fi

  if [[ ! -x "/bin/zsh" ]]; then
    sudo pacman -S --noconfirm --needed zsh
  fi

  for script_path in "$_DOTFILES_HOME"/migrations/*; do
    if ! grep -q "$script_path" "$migration_data"; then
      echo -e "\e[1;32mEXECUTING:\e[0m $script_path"
      $script_path
      echo "$script_path" >> "$migration_data"
    else 
      echo -e "\e[1;31mSKIPPING:\e[0m $script_path"
    fi
  done
}

function make_new_script() {
  file_name="$(date +%s)__$1.sh"
  script_path="$_DOTFILES_HOME/migrations/$file_name"

  {
    echo "#!/bin/zsh"
    echo ""
    echo "set -e"
    echo ""
    echo "source \"\$_DOTFILES_HOME/zsh/zshenv\""
    echo ""
  } >> "$script_path"
  
  chmod +x "$script_path"

  echo "$script_path"
}

if [[ "$1" != "" ]]; then
  case "$1" in
    help)
      print_usage
      exit 0
      ;;

    run)
      run_scripts
      exit 0
      ;;

    new)
      if [[ "$2" != "" ]]; then
        make_new_script "$2"
        exit 0
      else
        echo "Missing migration name!"
        echo ""
      fi
      ;;

  esac
fi

print_usage
exit 1
