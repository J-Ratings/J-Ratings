from __future__ import annotations
import re

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


PUBLIC_PRIVATE_DIRECTORIES = [
    Path("EuropeanFootball/compare"),
    Path("Go/compare"),
    Path("InternationalFootball/compare"),
    Path("RugbyUnion/compare"),
    Path("Snooker/compare"),
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


    for rel in PUBLIC_PRIVATE_DIRECTORIES:
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



SPORT_PUBLIC_FILTERS = {
    "EuropeanFootball": {
        "dated_dirs": ["data/history", "data/games"],
        "season_files": ["data/season_starts.json"],
    },
    "Go": {
        "dated_dirs": ["data/history", "data/games"],
        "season_files": ["data/era_starts.json"],
    },
    "InternationalFootball": {
        "dated_dirs": ["data/history", "data/games"],
        "season_files": ["data/era_starts.json"],
    },
    "RugbyUnion": {
        "dated_dirs": ["data/history", "data/games"],
        "season_files": ["data/era_starts.json"],
    },
    "Snooker": {
        "dated_dirs": ["data/history", "data/games"],
        "season_files": [],
    },
}


def filter_sport_public_data(sport: str, config: dict) -> None:
    sport_dir = OUT_DIR / sport

    removed_dated = 0
    removed_seasons = 0

    for rel_dir in config.get("dated_dirs", []):
        folder = sport_dir / rel_dir
        if not folder.exists():
            continue

        for path in folder.glob("*.json"):
            removed_dated += filter_rows_by_date(path)

    for rel_file in config.get("season_files", []):
        path = sport_dir / rel_file
        if path.exists():
            removed_seasons += filter_season_starts(path)

    print(f"{sport} dated rows removed before 2010: {removed_dated}")
    if config.get("season_files"):
        print(f"{sport} season/era rows removed before 2010: {removed_seasons}")


def filter_snooker_snapshots() -> int:
    snapshots_dir = OUT_DIR / "Snooker" / "data" / "snapshots"

    if not snapshots_dir.exists():
        return 0

    removed = 0

    for path in snapshots_dir.glob("*.json"):
        year_text = path.stem.strip()

        # current.json is the live/current snapshot and must remain public.
        if year_text.lower() == "current":
            continue

        # seasons.json is an index of available snapshot years.
        if year_text.lower() == "seasons":
            seasons = load_json(path)

            if not isinstance(seasons, list):
                raise RuntimeError(f"Expected a JSON list: {path}")

            filtered = []
            for value in seasons:
                try:
                    year = int(value)
                except (TypeError, ValueError):
                    continue

                if year >= 2010:
                    filtered.append(year)

            save_json(path, filtered)
            continue

        if not year_text.isdigit():
            raise RuntimeError(
                f"Unexpected Snooker snapshot filename: {path.name}"
            )

        if int(year_text) < 2010:
            path.unlink()
            removed += 1

    print(f"Snooker snapshot files removed before 2010: {removed}")
    return removed


def assert_no_pre_2010_sport_data(sport: str, config: dict) -> None:
    sport_dir = OUT_DIR / sport

    for rel_dir in config.get("dated_dirs", []):
        folder = sport_dir / rel_dir
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

    for rel_file in config.get("season_files", []):
        path = sport_dir / rel_file
        if not path.exists():
            continue

        data = load_json(path)

        if not isinstance(data, list):
            continue

        for row in data:
            if not isinstance(row, dict):
                continue

            year = season_start_year(row.get("season", ""))

            if year is not None and year < 2010:
                raise RuntimeError(
                    "SAFETY CHECK FAILED: pre-2010 season/era found in "
                    f"{path}: {row.get('season')}"
                )

    print(f"{sport} public-data safety checks: PASS")


def assert_no_pre_2010_snooker_snapshots() -> None:
    snapshots_dir = OUT_DIR / "Snooker" / "data" / "snapshots"

    if not snapshots_dir.exists():
        return

    for path in snapshots_dir.glob("*.json"):
        year_text = path.stem.strip()

        if year_text.lower() == "current":
            continue

        if year_text.lower() == "seasons":
            seasons = load_json(path)

            if not isinstance(seasons, list):
                raise RuntimeError(f"Expected a JSON list: {path}")

            for value in seasons:
                try:
                    year = int(value)
                except (TypeError, ValueError):
                    continue

                if year < 2010:
                    raise RuntimeError(
                        f"SAFETY CHECK FAILED: pre-2010 Snooker season found in {path}: {year}"
                    )
            continue

        if not year_text.isdigit():
            raise RuntimeError(
                f"SAFETY CHECK FAILED: unexpected Snooker snapshot filename: {path.name}"
            )

        if int(year_text) < 2010:
            raise RuntimeError(
                f"SAFETY CHECK FAILED: pre-2010 Snooker snapshot exists: {path}"
            )

    print("Snooker snapshot safety checks: PASS")


def assert_european_football_private_paths_absent() -> None:
    for rel in EUROPEAN_FOOTBALL_PRIVATE_PATHS:
        if (OUT_DIR / rel).exists():
            raise RuntimeError(
                f"SAFETY CHECK FAILED: private path exists in public build: {rel}"
            )

    print("European Football private-path safety checks: PASS")



def remove_html_block(text: str, start_marker: str, end_marker: str) -> str:
    start = text.find(start_marker)
    if start == -1:
        return text

    end = text.find(end_marker, start)
    if end == -1:
        raise RuntimeError(
            f"Could not find end marker while cleaning public HTML: {end_marker}"
        )

    end += len(end_marker)
    return text[:start] + text[end:]


def clean_european_football_public_ui() -> None:
    ef_dir = OUT_DIR / "EuropeanFootball"

    # -----------------------------
    # Home page
    # -----------------------------
    home = ef_dir / "home" / "index.html"
    if home.exists():
        s = home.read_text(encoding="utf-8")

        s = s.replace('      <a href="../compare/">Compare</a>\n', '')

        s = re.sub(
            r'\n\s*<button id="compare-btn"[^>]*>Compare</button>\s*',
            '\n',
            s,
            count=1,
        )

        # Remove the embedded comparison chart block, stopping immediately
        # before the normal ratings table.
        s, n = re.subn(
            r'\n\s*<div id="compare-wrap"[\s\S]*?(?=\n\s*<div class="table-wrap">)',
            '\n',
            s,
            count=1,
        )
        if n != 1:
            raise RuntimeError("Could not remove European Football home comparison chart")

        # Remove the comparison selection modal.
        s, n = re.subn(
            r'\n\s*<!-- Compare Selection Modal -->[\s\S]*?(?=\n\s*<footer class="site-footer">)',
            '\n',
            s,
            count=1,
        )
        if n != 1:
            raise RuntimeError("Could not remove European Football home comparison modal")

        # Prevent dead comparison setup from running.
        s = re.sub(r'\n\s*setupCompareButton\(\);\s*', '\n', s)
        s = re.sub(r'\n\s*setupCompareChartControls\(\);\s*', '\n', s)

        # The normal table/filter code still calls hideCompare(). Keep that
        # helper, but make it safe now that the public comparison DOM is gone.
        s = re.sub(
            r'function hideCompare\(\) \{[\s\S]*?\n\s*\}',
            """function hideCompare() {
    const wrap = document.getElementById('compare-wrap');
    const chart = document.getElementById('compare-chart');
    if (wrap) wrap.style.display = 'none';
    if (chart) chart.innerHTML = '';
    lastCompared = [];
    compareVisibleSeries = [];
  }""",
            s,
            count=1,
        )

        # Remove chart initialisation that references deleted comparison DOM.
        s = re.sub(
            r'\n\s*compareChart\s*=\s*JRGraph\.createChart\(\{[\s\S]*?\}\);\s*',
            '\n',
            s,
            count=1,
        )

        home.write_text(s, encoding="utf-8", newline="\n")
        print("Cleaned public European Football home UI")

    # -----------------------------
    # Peak page
    # -----------------------------
    peak = ef_dir / "peak" / "index.html"
    if peak.exists():
        s = peak.read_text(encoding="utf-8")

        s = s.replace('      <a href="../compare/">Compare</a>\n', '')

        s = re.sub(
            r'\n\s*<button id="compare-btn"[^>]*>Compare</button>\s*',
            '\n',
            s,
            count=1,
        )

        s, n = re.subn(
            r'\n\s*<div id="compare-wrap"[\s\S]*?(?=\n\s*<div class="table-wrap">)',
            '\n',
            s,
            count=1,
        )
        if n != 1:
            raise RuntimeError("Could not remove European Football peak comparison chart")

        # Peak has no footer immediately after the modal, so stop at <script>.
        s, n = re.subn(
            r'\n\s*<div id="compare-modal">[\s\S]*?(?=\n\s*<script>)',
            '\n',
            s,
            count=1,
        )
        if n != 1:
            raise RuntimeError("Could not remove European Football peak comparison modal")

        s = re.sub(r'\n\s*setupCompareButton\(\);\s*', '\n', s)
        s = re.sub(r'\n\s*setupCompareChartControls\(\);\s*', '\n', s)

        peak.write_text(s, encoding="utf-8", newline="\n")
        print("Cleaned public European Football peak UI")

    # -----------------------------
    # Player page
    # -----------------------------
    player = ef_dir / "player" / "index.html"
    if player.exists():
        s = player.read_text(encoding="utf-8")

        s = s.replace('      <a href="../compare/">Compare</a>\n', '')

        # Remove the private Statistics card/link.
        s, n = re.subn(
            r'\n\s*<a id="statsLink" class="stat stat-link" href="\./stats/">[\s\S]*?</a>',
            '',
            s,
            count=1,
        )
        if n != 1:
            raise RuntimeError("Could not remove European Football player Statistics link")

        # Remove JS that rewrites the Statistics link.
        s, n = re.subn(
            r'\n\s*const statsLink = document\.getElementById\(\'statsLink\'\);'
            r'\s*if \(statsLink && idRaw\) \{\s*'
            r'statsLink\.href = `\./stats/\?id=\$\{encodeURIComponent\(idRaw\)\}`;\s*\}',
            '\n',
            s,
            count=1,
        )
        if n != 1:
            raise RuntimeError("Could not remove European Football player Statistics JS")

        # Public detailed history begins in 2010.
        s = re.sub(
            r'\s*<button type="button" class="range-btn seg" data-range="ALL">All</button>\s*',
            '\n            ',
            s,
            count=1,
        )
        s = s.replace(
            '<button type="button" class="range-btn seg is-active active" data-range="2000">Since 2000</button>',
            '<button type="button" class="range-btn seg is-active active" data-range="2010">Since 2010</button>',
            1,
        )
        s = s.replace("defaultRange: '2000'", "defaultRange: '2010'", 1)

        player.write_text(s, encoding="utf-8", newline="\n")
        print("Cleaned public European Football player UI")


def assert_european_football_public_ui_clean() -> None:
    checks = [
        (OUT_DIR / "EuropeanFootball" / "home" / "index.html", '../compare/', 'home Compare link'),
        (OUT_DIR / "EuropeanFootball" / "peak" / "index.html", '../compare/', 'peak Compare link'),
        (OUT_DIR / "EuropeanFootball" / "player" / "index.html", '../compare/', 'player Compare link'),
        (OUT_DIR / "EuropeanFootball" / "player" / "index.html", './stats/', 'player Statistics link'),
    ]

    for path, needle, label in checks:
        if not path.exists():
            continue
        s = path.read_text(encoding="utf-8")
        if needle in s:
            raise RuntimeError(
                f"SAFETY CHECK FAILED: public UI still contains {label}: {path}"
            )

    player = OUT_DIR / "EuropeanFootball" / "player" / "index.html"
    if player.exists():
        s = player.read_text(encoding="utf-8")
        if 'data-range="2000"' in s or "defaultRange: '2000'" in s:
            raise RuntimeError(
                "SAFETY CHECK FAILED: public player page still advertises pre-2010 range"
            )

    home = OUT_DIR / "EuropeanFootball" / "home" / "index.html"
    if home.exists():
        s = home.read_text(encoding="utf-8")
        if "compareChart = JRGraph.createChart" in s:
            raise RuntimeError(
                "SAFETY CHECK FAILED: public European Football home still initialises compare chart"
            )
        if "document.getElementById('compare-wrap').style" in s:
            raise RuntimeError(
                "SAFETY CHECK FAILED: public European Football home has unsafe compare-wrap access"
            )

    print("European Football public-UI safety checks: PASS")



PUBLIC_SPORTS = [
    "EuropeanFootball",
    "Go",
    "InternationalFootball",
    "RugbyUnion",
    "Snooker",
]


def clean_generic_public_ui() -> None:
    for sport in PUBLIC_SPORTS:
        sport_dir = OUT_DIR / sport
        if not sport_dir.exists():
            continue

        for html in sport_dir.rglob("*.html"):
            s = html.read_text(encoding="utf-8")

            # Remove links to the private Compare section.
            s = re.sub(
                r'\s*<a\b[^>]*href=["\'][^"\']*compare/[^"\']*["\'][^>]*>\s*Compare\s*</a>\s*',
                '\n',
                s,
                flags=re.I,
            )

            # No public comparison feature for now: remove the visible launch
            # button from Home/Peak-style pages across every sport.
            s = re.sub(
                r'\s*<button\b[^>]*id=["\']compare-btn["\'][^>]*>[\s\S]*?</button>\s*',
                '\n',
                s,
                count=1,
                flags=re.I,
            )

            # Remove calls that initialise the now-hidden Compare button.
            # Otherwise pages can fail before the normal ratings table loads.
            s = re.sub(
                r'\n\s*setupCompareButton\(\);\s*',
                '\n',
                s,
            )

            # Public detailed history begins in 2010.
            s = s.replace('data-range="2000"', 'data-range="2010"')
            s = s.replace("data-range='2000'", "data-range='2010'")
            s = s.replace('data-since="2000"', 'data-since="2010"')
            s = s.replace("data-since='2000'", "data-since='2010'")
            s = s.replace("Since 2000", "Since 2010")
            s = s.replace("defaultRange: '2000'", "defaultRange: '2010'")
            s = s.replace('defaultRange: "2000"', 'defaultRange: "2010"')

            html.write_text(s, encoding="utf-8", newline="\n")

    print("Generic public UI cleanup applied across all sports")


def assert_generic_public_ui_clean() -> None:
    failures = []

    for sport in PUBLIC_SPORTS:
        sport_dir = OUT_DIR / sport
        if not sport_dir.exists():
            continue

        if (sport_dir / "compare").exists():
            failures.append(f"{sport}: private compare directory still exists")

        for html in sport_dir.rglob("*.html"):
            s = html.read_text(encoding="utf-8")
            rel = html.relative_to(OUT_DIR).as_posix()

            if re.search(r'href=["\'][^"\']*compare/', s, flags=re.I):
                failures.append(f"{rel}: link to private compare section")

            if re.search(r'id=["\']compare-btn["\']', s, flags=re.I):
                failures.append(f"{rel}: visible Compare button remains")

            if re.search(r'\bsetupCompareButton\(\);', s):
                failures.append(f"{rel}: Compare-button initialiser remains")

            if (
                'data-range="2000"' in s
                or "data-range='2000'" in s
                or 'data-since="2000"' in s
                or "data-since='2000'" in s
                or "Since 2000" in s
                or "defaultRange: '2000'" in s
                or 'defaultRange: "2000"' in s
            ):
                failures.append(f"{rel}: pre-2010 UI range remains")

    if failures:
        raise RuntimeError(
            "PUBLIC UI SAFETY CHECK FAILED:\n  " + "\n  ".join(failures)
        )

    print("Generic public-UI safety checks: PASS")


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
    assert_european_football_private_paths_absent()
    clean_european_football_public_ui()
    assert_european_football_public_ui_clean()
    clean_generic_public_ui()
    assert_generic_public_ui_clean()

    for sport, config in SPORT_PUBLIC_FILTERS.items():
        filter_sport_public_data(sport, config)

    filter_snooker_snapshots()

    for sport, config in SPORT_PUBLIC_FILTERS.items():
        assert_no_pre_2010_sport_data(sport, config)

    assert_no_pre_2010_snooker_snapshots()

    print()
    print(f"Public-site build created at: {OUT_DIR}")
    print("All configured sports have had detailed historical data filtered to 2010+.")
    print("Public data and UI safety checks: PASS")


if __name__ == "__main__":
    build_public_site()
