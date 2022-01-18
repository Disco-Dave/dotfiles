#!/bin/bash

path_to_yaml_merge=$(which yaml-merge)

if [ ! -x "$path_to_yaml_merge" ]; then
    >&2 echo "Downloading and installing yaml-merge"

    (
        cd /tmp

        if [ ! -d "yaml-merge" ]; then
            git clone https://github.com/Disco-Dave/yaml-merge 
        fi

        cd yaml-merge
        stack install
    )
fi

host_name_config="$(hostnamectl hostname).yml"

if [ -f "$host_name_config" ]; then
    >&2 echo "Overlaying host specific settings"
    yaml-merge "$host_name_config" base.yml > alacritty.yml
else
    cp base.yml alacritty.yml
fi
