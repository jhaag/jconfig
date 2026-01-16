import json
import urllib.request
import os
import time
import fcntl
import subprocess
import sys
from ..utils import BasicSegment

CACHE_FILE = os.path.join(os.path.dirname(__file__), ".outside.json")


def load_cache() -> dict[str, str]:
    try:
        with open(CACHE_FILE, "r") as f:
            return json.load(f)
    except (FileNotFoundError, json.JSONDecodeError):
        return {}


def save_cache(data: dict[str, str]) -> None:
    print(CACHE_FILE)
    with open(CACHE_FILE, "w") as f:
        json.dump(data, f)


def fetch_location() -> dict[str, str] | None:
    try:
        with urllib.request.urlopen("http://ipapi.co/json/", timeout=5) as response:
            location_data = json.loads(response.read().decode())
        if "error" in location_data:
            return None
        return location_data
    except Exception:
        return None


def fetch_weather(latitude: str, longitude: str) -> str:
    try:
        weather_url = f"http://wttr.in/{latitude},{longitude}?format=%C"
        with urllib.request.urlopen(weather_url, timeout=5) as response:
            return response.read().decode().strip()
    except Exception:
        return None


class Segment(BasicSegment):
    def add_to_powerline(self):
        cache = load_cache()
        now = time.time()
        loc_data = None
        weather_data = None

        # Load cached location if fresh (<24 hours)
        if "location" in cache and now - cache["location"]["timestamp"] < 24 * 3600:
            loc_data = cache["location"]["data"]

        # Load cached weather if fresh (<5 minutes)
        if "weather" in cache and now - cache["weather"]["timestamp"] < 5 * 60:
            weather_data = cache["weather"]["data"]

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
