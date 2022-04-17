#!/bin/bash

PROFILE_PATH="$(realpath ~/.mozilla/firefox/*.privacy/)"

"$PROFILE_PATH/updater.sh" -s
"$PROFILE_PATH/prefsCleaner.sh" -s
