#!/bin/bash

WALLPAPER_HOME="${HOME}/Wallpapers"

mkdir -p "${WALLPAPER_HOME}"

# Set $1 to monitor dimensions
set $(xrandr | grep '*' | head -1)
DIMENSIONS="$1"

echo "Downloading images to ${WALLPAPER_HOME} at ${DIMENSIONS}"
interfacelift-downloader "${DIMENSIONS}" "${WALLPAPER_HOME}"
