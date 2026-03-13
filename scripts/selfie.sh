#!/usr/bin/env bash
# selfie.sh — Screenshot the caller's window. Saves to /tmp, prints the path.
# Dependency: gnome-screenshot (apt install gnome-screenshot)

set -euo pipefail

OUTFILE="/tmp/selfie_$(date +%Y%m%d_%H%M%S_%3N).png"

gnome-screenshot --window --include-pointer --file="$OUTFILE"

echo "$OUTFILE"
