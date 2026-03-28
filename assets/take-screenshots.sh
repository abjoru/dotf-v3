#!/usr/bin/env bash
# Takes screenshots of all 3 dotf TUI tabs.
# Usage: ./assets/take-screenshots.sh
#
# Requires: dotool, grim, hyprctl, jq, alacritty

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
EMPTY_WS="99"
TERM_COLS=120
TERM_ROWS=35

# Remember current workspace
CURRENT_WS=$(hyprctl activeworkspace -j | jq -r '.id')

echo "Current workspace: $CURRENT_WS"
echo "Switching to empty workspace $EMPTY_WS..."

# Float alacritty windows on this workspace
hyprctl keyword windowrule 'float on, match:title ^(dotf-screenshot)$'
hyprctl keyword windowrule 'size 1400 900, match:title ^(dotf-screenshot)$'
hyprctl keyword windowrule 'center on, match:title ^(dotf-screenshot)$'

hyprctl dispatch workspace "$EMPTY_WS"
sleep 0.5

# Launch dotf in a sized alacritty terminal
echo "Launching dotf TUI..."
alacritty --title dotf-screenshot -o "window.dimensions.columns=$TERM_COLS" -o "window.dimensions.lines=$TERM_ROWS" -e dotf &
TERM_PID=$!
sleep 2

# Helper: capture the alacritty window
capture() {
  local name="$1"
  local geom
  geom=$(hyprctl clients -j | jq -r '.[] | select(.title == "dotf-screenshot") | "\(.at[0]),\(.at[1]) \(.size[0])x\(.size[1])"' | head -1)
  if [ -n "$geom" ] && [ "$geom" != "null" ]; then
    echo "Capturing $name (geometry: $geom)..."
    grim -g "$geom" "$SCRIPT_DIR/$name"
  else
    echo "WARNING: Could not find window for $name, capturing full screen"
    grim "$SCRIPT_DIR/$name"
  fi
}

# Tab 1: Dotfiles (default tab)
sleep 1
capture "screenshot-dotfiles.png"

# Tab 2: Plugins
echo "Switching to Plugins tab..."
echo "key 2" | dotool
sleep 1
capture "screenshot-plugins.png"

# Tab 3: Profiles
echo "Switching to Profiles tab..."
echo "key 3" | dotool
sleep 1
capture "screenshot-profiles.png"

# Quit dotf
echo "key q" | dotool
sleep 0.5

# Cleanup
echo "Cleaning up..."
kill "$TERM_PID" 2>/dev/null || true
sleep 0.3
hyprctl dispatch workspace "$CURRENT_WS"

echo "Screenshots saved to: $SCRIPT_DIR/"
