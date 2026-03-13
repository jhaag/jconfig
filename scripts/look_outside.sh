#!/usr/bin/env bash
# look_outside.sh - Fetch location and weather data
# Usage: look_outside.sh <output_file>

set -euo pipefail

# Check if output file is provided
if [[ $# -lt 1 ]]; then
    echo "Usage: $0 <output_file>" >&2
    exit 1
fi

OUTPUT_FILE="$1"

# Check if nmcli is available
if ! command -v nmcli &>/dev/null; then
    echo "Error: nmcli not found" >&2
    exit 1
fi

# Check if connected to VPN using nmcli
# Exit early with 0 if VPN is active
if nmcli connection show --active | grep -qiE 'vpn|tun|wireguard'; then
    exit 0
fi

# Fetch location data
location_data=$(curl -sf --max-time 5 "https://ipapi.co/json/" || echo "")

if [[ -z "$location_data" ]]; then
    exit 1
fi

# Check for error in response
if echo "$location_data" | jq -e '.error' &>/dev/null; then
    exit 1
fi

# Extract latitude and longitude
latitude=$(echo "$location_data" | jq -r '.latitude // empty')
longitude=$(echo "$location_data" | jq -r '.longitude // empty')

if [[ -z "$latitude" ]] || [[ -z "$longitude" ]]; then
    exit 1
fi

# Extract location information
country_code=$(echo "$location_data" | jq -r '.country // ""')

# Fetch weather data
weather=$(curl -sf --max-time 5 "https://wttr.in/${latitude},${longitude}?format=%C" || echo "")

if [[ -z "$weather" ]]; then
    weather="Unknown"
fi

# Create JSON output
datum=$(jq -n \
    --arg country_code "$country_code" \
    --arg weather "$weather" \
    '{location: {country_code: $country_code}, weather: $weather}')

# Write to output file (overwrite)
echo "$datum" > "$OUTPUT_FILE"
