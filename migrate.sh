#!/usr/bin/env bash

set -e

function print_help() {
  echo "Migrate dotfiles by applying incremental scripts"
  echo
  echo "Syntax: migrate.sh run | migrate.sh new NAME_OF_MIGRATION"
  echo "options:"
  echo "run     Migrate to the latest revisions of dotfiles"
  echo "new     Create a new migration script"
  echo
}

script_path="$(realpath "$0")"
dotfiles="$(dirname "$script_path")"

cd "$dotfiles"

if [[ "$1" == "run" ]]; then
  echo "You ran run"
elif [[ "$1" == "new" ]]; then
  migration_file="./migrations/$(date --utc +%s)__$2.sh"
  echo "#!/usr/bin/env bash" >> "$migration_file"
  echo >> "$migration_file"
  chmod +x "$migration_file"
  echo "$migration_file"
else
  print_help
fi
