from __future__ import annotations

import json
import shutil
from pathlib import Path

PUBLIC_START = "2010-01-01"

SCRIPT_DIR = Path(__file__).resolve().parent
REPO_DIR = SCRIPT_DIR.parents[1]
OUT_DIR = REPO_DIR / "_public_site"

# Top-level folders that are part of the website.
PUBLIC_TOP_LEVEL_DIRS = [
    "core",
    "EuropeanFootball",
    "flags",
    "Go",
    "InternationalFootball",
    "RugbyUnion",
    "site",
    "Snooker",
    "sports",
]

# Root-level website files/assets.
PUBLIC_ROOT_FILES = [
    ".nojekyll",
    "CNAME",
    "index.html",
    "jratings-graph.js",
    "logo.png",
    "style.css",
    "Baseball Button.png",
    "Basketball Button.png",
    "Chess Banner Logo.png",
    "Chess Button.png",
    "Cricket Banner Logo.png",
    "Cricket Button.png",
    "Football Banner Logo.png",
    "Football Button.png",
    "Go Banner Logo.png",
    "Go Button.png",
    "NFL Banner Logo.png",
    "NFL Button.png",
    "Rugby Banner Logo.png",
    "Rugby Button.png",
    "Snooker Banner Logo.png",
    "Snooker Button.png",
]

# Never publish these directory names anywhere in the copied site.
GENERIC_PRIVATE_DIR_NAMES = {
    ".git",
    ".github",
    "pipeline_data",
    "outputs",
    "__pycache__",
}

# European Football advanced/private areas.
EUROPEAN_FOOTBALL_PRIVATE_PATHS = [
    Path("EuropeanFootball/compare"),
    Path("EuropeanFootball/playback"),
    Path("EuropeanFootball/simulate"),
    Path("EuropeanFootball/player/stats"),
    Path("EuropeanFootball/data/playback"),
    Path("EuropeanFootball/data/simulations"),
Path("EuropeanFootball/data/top_teams.json"),
Path("EuropeanFootball/Planned Competition.xlsx"),
Path("EuropeanFootball/premier_league_simulator_prototype.html"),
]


def should_ignore_dir(path: Path) -> bool:
    return any(part in GENERIC_PRIVATE_DIR_NAMES for part in path.parts)


def copy_tree_filtered(src: Path, dst: Path) -> None:
    for item in src.rglob("*"):
        rel = item.relative_to(src)

        if should_ignore_dir(rel):
            continue

        target = dst / rel

        if item.is_dir():
            target.mkdir(parents=True, exist_ok=True)
        elif item.is_file():
            target.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(item, target)


def remove_private_paths() -> None:
    for rel in EUROPEAN_FOOTBALL_PRIVATE_PATHS:
        target = OUT_DIR / rel

        if target.is_dir():
            shutil.rmtree(target)
            print(f"Removed private directory: {rel.as_posix()}")
        elif target.is_file():
            target.unlink()
            print(f"Removed private file: {rel.as_posix()}")


def load_json(path: Path):
    with path.open("r", encoding="utf-8") as f:
        return json.load(f)


def save_json(path: Path, data) -> None:
    with path.open("w", encoding="utf-8", newline="\n") as f:
        json.dump(data, f, ensure_ascii=False, separators=(",", ":"))


def filter_rows_by_date(path: Path) -> int:
    data = load_json(path)

    if not isinstance(data, list):
        raise RuntimeError(f"Expected a JSON list: {path}")

    before = len(data)

    filtered = []
    for row in data:
        if not isinstance(row, dict):
            continue

        date = str(row.get("date", "")).strip()

        # Keep future/upcoming rows and all rows from 2010 onwards.
        if date and date >= PUBLIC_START:
            filtered.append(row)

    save_json(path, filtered)
    return before - len(filtered)


def season_start_year(value: str) -> int | None:
    value = str(value or "").strip()
    if len(value) < 4 or not value[:4].isdigit():
        return None
    return int(value[:4])


def filter_season_starts(path: Path) -> int:
    data = load_json(path)

    if not isinstance(data, list):
        raise RuntimeError(f"Expected a JSON list: {path}")

    before = len(data)

    filtered = []
    for row in data:
        if not isinstance(row, dict):
            continue

        year = season_start_year(row.get("season", ""))
        if year is not None and year >= 2010:
            filtered.append(row)

    save_json(path, filtered)
    return before - len(filtered)


def filter_european_football_public_data() -> None:
    ef_data = OUT_DIR / "EuropeanFootball" / "data"

    history_dir = ef_data / "history"
    games_dir = ef_data / "games"

    removed_history = 0
    removed_games = 0

    if history_dir.exists():
        for path in history_dir.glob("*.json"):
            removed_history += filter_rows_by_date(path)

    if games_dir.exists():
        for path in games_dir.glob("*.json"):
            removed_games += filter_rows_by_date(path)

    season_starts = ef_data / "season_starts.json"
    removed_seasons = 0

    if season_starts.exists():
        removed_seasons = filter_season_starts(season_starts)

    print(f"European Football history rows removed before 2010: {removed_history}")
    print(f"European Football game rows removed before 2010: {removed_games}")
    print(f"European Football season-start rows removed before 2010: {removed_seasons}")


def assert_no_pre_2010_european_football_data() -> None:
    ef_data = OUT_DIR / "EuropeanFootball" / "data"

    for folder_name in ("history", "games"):
        folder = ef_data / folder_name
        if not folder.exists():
            continue

        for path in folder.glob("*.json"):
            data = load_json(path)

            if not isinstance(data, list):
                continue

            for row in data:
                if not isinstance(row, dict):
                    continue

                date = str(row.get("date", "")).strip()

                if date and date < PUBLIC_START:
                    raise RuntimeError(
                        f"SAFETY CHECK FAILED: pre-2010 row found in {path}: {date}"
                    )

    season_starts = ef_data / "season_starts.json"
    if season_starts.exists():
        data = load_json(season_starts)

        if isinstance(data, list):
            for row in data:
                if not isinstance(row, dict):
                    continue

                year = season_start_year(row.get("season", ""))

                if year is not None and year < 2010:
                    raise RuntimeError(
                        "SAFETY CHECK FAILED: pre-2010 season found in "
                        f"{season_starts}: {row.get('season')}"
                    )

    for rel in EUROPEAN_FOOTBALL_PRIVATE_PATHS:
        if (OUT_DIR / rel).exists():
            raise RuntimeError(
                f"SAFETY CHECK FAILED: private path exists in public build: {rel}"
            )

    print("European Football public-data safety checks: PASS")


def build_public_site() -> None:
    if OUT_DIR.exists():
        shutil.rmtree(OUT_DIR)

    OUT_DIR.mkdir(parents=True)

    for filename in PUBLIC_ROOT_FILES:
        src = REPO_DIR / filename
        if src.exists():
            shutil.copy2(src, OUT_DIR / filename)

    for dirname in PUBLIC_TOP_LEVEL_DIRS:
        src = REPO_DIR / dirname
        dst = OUT_DIR / dirname

        if not src.exists():
            print(f"Warning: top-level folder not found: {dirname}")
            continue

        copy_tree_filtered(src, dst)

    remove_private_paths()
    filter_european_football_public_data()
    assert_no_pre_2010_european_football_data()

    print()
    print(f"Public-site build created at: {OUT_DIR}")
    print("IMPORTANT: other sports have not yet had their historical JSON filtered.")
    print("Do not deploy this build yet.")


if __name__ == "__main__":
    build_public_site()
