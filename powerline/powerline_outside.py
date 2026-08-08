import json
import urllib.request
import os
import time
import fcntl
import subprocess
import sys

from pathlib import Path

from ..utils import BasicSegment


class Segment(BasicSegment):
    def add_to_powerline(self):
        cache = self.load_cache()

        if cache is None:
            # No fresh cache - show creative filler
            half_space = "\u2009"
            segment_text = f"🏠 {half_space}🤷"
            bg = self.powerline.theme.VIRTUAL_ENV_BG
            fg = self.powerline.theme.VIRTUAL_ENV_FG
            self.powerline.append(" " + segment_text + f"{half_space} ", fg, bg)
            return

        loc_data = cache.get("location")
        weather_data = cache.get("weather")

        # Determine globe emoji (default North America)
        country_code = loc_data.get("country_code", "") if loc_data else ""
        if country_code in [
            "US",
            "CA",
            "MX",
            "BR",
            "AR",
            "CL",
            "CO",
            "PE",
            "VE",
            "EC",
            "BO",
            "PY",
            "UY",
            "GY",
            "SR",
            "GF",
            "JM",
            "HT",
            "DO",
            "PR",
            "CU",
            "BS",
            "BZ",
            "CR",
            "SV",
            "GT",
            "HN",
            "NI",
            "PA",
        ]:
            globe_emoji = "🌎"
        elif country_code in [
            "CN",
            "JP",
            "KR",
            "IN",
            "ID",
            "VN",
            "TH",
            "MY",
            "PH",
            "SG",
            "TW",
            "HK",
            "AU",
            "NZ",
            "PK",
            "BD",
            "LK",
            "NP",
            "KH",
            "LA",
            "MM",
            "MN",
            "KP",
            "BT",
            "TL",
            "FJ",
            "PG",
            "SB",
            "VU",
            "WS",
            "TO",
            "TV",
            "KI",
            "MH",
            "FM",
            "PW",
            "NR",
        ]:
            globe_emoji = "🌏"
        else:
            globe_emoji = (
                "🌍" if country_code else "🌎"
            )  # Default North America if no country

        # Determine weather emoji (default sunny)
        weather_condition = weather_data if weather_data else "Sunny"
        weather_mapping = {
            "Sunny": "☀️",
            "Clear": "☀️",
            "Partly cloudy": "⛅",
            "Cloudy": "☁️",
            "Overcast": "☁️",
            "Mist": "🌫️",
            "Fog": "🌫️",
            "Light rain": "🌦️",
            "Moderate rain": "🌧️",
            "Heavy rain": "🌧️",
            "Light snow": "🌨️",
            "Moderate snow": "❄️",
            "Heavy snow": "❄️",
            "Thunderstorm": "⛈️",
            "Patchy rain possible": "🌦️",
            "Patchy snow possible": "🌨️",
            "Patchy sleet possible": "🌨️",
            "Patchy freezing drizzle possible": "🌨️",
            "Thundery outbreaks possible": "⛈️",
            "Blowing snow": "❄️",
            "Blizzard": "❄️",
            "Freezing fog": "🌫️",
            "Freezing drizzle": "🌨️",
            "Heavy freezing drizzle": "🌨️",
            "Light drizzle": "🌦️",
            "Heavy drizzle": "🌧️",
            "Light sleet": "🌨️",
            "Moderate or heavy sleet": "🌨️",
            "Light showers of ice pellets": "🌨️",
            "Moderate or heavy showers of ice pellets": "🌨️",
            "Ice pellets": "🌨️",
        }
        weather_emoji = weather_mapping.get(weather_condition, "🌤️")

        # Always display the segment
        half_space = "\u2009"
        segment_text = f"{globe_emoji} {half_space}{weather_emoji}"
        bg = self.powerline.theme.VIRTUAL_ENV_BG
        fg = self.powerline.theme.VIRTUAL_ENV_FG
        self.powerline.append(" " + segment_text + f"{half_space} ", fg, bg)

    def load_cache(self) -> dict[str, str] | None:
        cache_file = Path(os.environ.get("JASPAH_ROOT", "~/jaspah")).expanduser().resolve() / ".outside.json"

        # Check if cache file is fresh (<5 minutes old)
        cache_is_fresh = False
        if cache_file.exists():
            file_age = time.time() - cache_file.stat().st_mtime
            cache_is_fresh = file_age < 5 * 60
            cache_is_stale = file_age < 3600 * 24

            if not cache_is_fresh:
                if cache_is_stale:
                    cache_file.unlink()
                return None

        try:
            with open(cache_file, "r") as f:
                return json.load(f)
        except (FileNotFoundError, json.JSONDecodeError):
            return None
