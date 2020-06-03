#!/bin/bash

set -e

(cd ./sources/xmonad-git; git pull)
(cd ./sources/xmonad-contrib-git; git pull)

stack install
