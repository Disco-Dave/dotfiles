#!/usr/bin/env bash

docker run --rm -it "$(docker build --file sandbox/Dockerfile -q .)" zsh
