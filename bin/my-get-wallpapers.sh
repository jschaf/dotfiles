#!/bin/bash
# Downloads wallpapers from interfacelift.
#
# Requires interfacelift-downloader from npm.  Install with:
#    npm install -g interfacelift-downloader

WALLPAPER_HOME="${HOME}/Wallpapers"
NUM_WALLPAPERS=1000

mkdir -p "${WALLPAPER_HOME}"

# Set $1 to monitor dimensions
set $(xrandr | grep '*' | head -1)
DIMENSIONS="$1"

echo "Downloading ${NUM_WALLPAPERS} images to ${WALLPAPER_HOME} at \
 ${DIMENSIONS}"
interfacelift-downloader "${DIMENSIONS}" "${WALLPAPER_HOME}" "${NUM_WALLPAPERS}"
