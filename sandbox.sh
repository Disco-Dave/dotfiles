#!/bin/bash

set -e

dotfiles_home="$(dirname "$(realpath "$0")")"
image_name="davids-dotfiles"

function print_usage() {
  cat << EOF
Build or run these dotfiles inside of docker
Usage: $0 COMMAND
Commands:
  -h,--help                       Print this help message
  build                           Build these dotfiles into a docker image named $image_name
  run [any docker run options]    Run a container with the davids-dotfile image
EOF
}

function build() {
  docker build --file sandbox/Dockerfile -t "$image_name" "$dotfiles_home"
}

function run() {
  if ! docker inspect --type=image "$image_name" &> /dev/null; then
    build
  fi

  docker run -it --hostname sandbox $@ "$image_name"
}

if [[ "$1" != "" ]]; then
  case "$1" in
    -h | --help)
      print_usage
      exit 0
      ;;

    run)
      shift
      run $@
      exit 0
      ;;

    build)
      build
      exit 0
      ;;

  esac
fi

print_usage
exit 1
