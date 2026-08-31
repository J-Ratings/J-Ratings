from __future__ import annotations
import re

import json
import shutil
import sys
from pathlib import Path

PUBLIC_START = "2010-01-01"

SCRIPT_DIR = Path(__file__).resolve().parent
REPO_DIR = SCRIPT_DIR.parents[1]
LIVE_OUT_DIR = REPO_DIR / "_public_site"
TEMP_OUT_DIR = REPO_DIR / "_public_site_build"
PREVIOUS_OUT_DIR = REPO_DIR / "_public_site_previous"

# Build and validate in a temporary directory. Only swap it into place after
# every public-data/UI safety check has passed. All helper functions below
# deliberately operate on OUT_DIR, which points at the temporary build.
OUT_DIR = TEMP_OUT_DIR

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


PUBLIC_TEASER_STYLE = """
  <style id="jr-public-teaser-styles">
    .jr-public-disabled {
      opacity: 0.42 !important;
      cursor: not-allowed !important;
      pointer-events: none !important;
      user-select: none !important;
      filter: saturate(0.55);
    }

    .jr-public-disabled[disabled] {
      opacity: 0.42 !important;
    }

    .topnav .jr-public-nav-teaser {
      white-space: nowrap;
    }

    .jr-public-stats-teaser .stat-value {
      font-size: 0.78rem !important;
      line-height: 1.15 !important;
    }
  </style>
"""


def add_classes_to_open_tag(tag: str, classes: str) -> str:
    class_match = re.search(r'\bclass=(["\'])(.*?)\1', tag, flags=re.I | re.S)

    if class_match:
        existing = class_match.group(2).split()
        for cls in classes.split():
            if cls not in existing:
                existing.append(cls)

        replacement = f'class={class_match.group(1)}{" ".join(existing)}{class_match.group(1)}'
        return tag[:class_match.start()] + replacement + tag[class_match.end():]

    return re.sub(
        r'^<([a-zA-Z0-9]+)\b',
        rf'<\1 class="{classes}"',
        tag,
        count=1,
    )


def ensure_public_teaser_styles(s: str) -> str:
    if 'id="jr-public-teaser-styles"' in s:
        return s

    if "</head>" in s:
        return s.replace("</head>", PUBLIC_TEASER_STYLE + "\n</head>", 1)

    return s


def make_compare_teasers(s: str) -> str:
    # Keep the Compare tab visible, but remove its destination completely.
    def nav_repl(match: re.Match) -> str:
        return (
            '<a class="jr-public-disabled jr-public-nav-teaser" '
            'aria-disabled="true" tabindex="-1" title="Coming soon">Compare</a>'
        )

    s = re.sub(
        r'<a\b[^>]*href=["\'][^"\']*compare/[^"\']*["\'][^>]*>\s*Compare\s*</a>',
        nav_repl,
        s,
        flags=re.I,
    )

    # Keep Home/Peak Compare launch buttons as non-functional previews.
    def button_repl(match: re.Match) -> str:
        opening = match.group(1)
        inner = match.group(2)

        opening = re.sub(
            r'\s+id=["\']compare-btn["\']',
            '',
            opening,
            count=1,
            flags=re.I,
        )
        opening = add_classes_to_open_tag(
            opening,
            "jr-public-disabled jr-public-compare-teaser",
        )

        opening = re.sub(r'\s+disabled(?:=["\'][^"\']*["\'])?', '', opening, flags=re.I)
        opening = re.sub(r'\s+aria-disabled=["\'][^"\']*["\']', '', opening, flags=re.I)
        opening = re.sub(r'\s+title=["\'][^"\']*["\']', '', opening, flags=re.I)

        opening = opening[:-1] + (
            ' disabled aria-disabled="true" title="Coming soon">'
        )

        return opening + inner + "</button>"

    s = re.sub(
        r'(<button\b(?=[^>]*\bid=["\']compare-btn["\'])[^>]*>)([\s\S]*?)</button>',
        button_repl,
        s,
        count=1,
        flags=re.I,
    )

    return s


def make_history_range_teasers(s: str) -> str:
    # Historical controls may remain visible as previews, but they must not
    # carry live data-range/data-since attributes that the JavaScript can use.
    button_re = re.compile(
        r'<button\b(?P<open>[^>]*)\bdata-(?P<kind>range|since)='
        r'(?P<quote>["\'])(?P<value>.*?)(?P=quote)(?P<tail>[^>]*)>'
        r'(?P<inner>[\s\S]*?)</button>',
        flags=re.I,
    )

    def repl(match: re.Match) -> str:
        kind = match.group("kind").lower()
        value = match.group("value").strip()
        value_upper = value.upper()

        private = False

        if kind == "range":
            if value_upper == "ALL":
                private = True
            elif value.isdigit() and int(value) < 2010:
                private = True
        elif kind == "since":
            if value == "":
                private = True
            elif value.isdigit() and int(value) < 2010:
                private = True

        if not private:
            return match.group(0)

        opening = "<button" + match.group("open") + match.group("tail") + ">"
        opening = add_classes_to_open_tag(
            opening,
            "jr-public-disabled jr-public-history-teaser",
        )

        # A disabled preview must not look selected.
        opening = re.sub(
            r'\bclass=(["\'])(.*?)\1',
            lambda m: (
                f'class={m.group(1)}'
                + " ".join(
                    cls for cls in m.group(2).split()
                    if cls not in {"active", "is-active"}
                )
                + m.group(1)
            ),
            opening,
            count=1,
            flags=re.I | re.S,
        )

        opening = re.sub(r'\s+disabled(?:=["\'][^"\']*["\'])?', '', opening, flags=re.I)
        opening = re.sub(r'\s+aria-disabled=["\'][^"\']*["\']', '', opening, flags=re.I)
        opening = re.sub(r'\s+title=["\'][^"\']*["\']', '', opening, flags=re.I)

        public_attr = "data-public-range"
        public_value = value if value else "ALL"

        opening = opening[:-1] + (
            f' {public_attr}="{public_value}"'
            ' disabled aria-disabled="true"'
            ' title="Full history coming soon">'
        )

        return opening + match.group("inner") + "</button>"

    s = button_re.sub(repl, s)

    # Any chart which previously defaulted to a private pre-2010 preset must
    # start on the first public year instead.
    s = re.sub(
        r'defaultRange:\s*(["\'])2000\1',
        "defaultRange: '2010'",
        s,
        flags=re.I,
    )
    s = re.sub(
        r'\b(SINCE_YEAR|COMPARE_SINCE_YEAR)\s*=\s*(["\']?)2000\2\s*;',
        lambda m: f"{m.group(1)} = {m.group(2)}2010{m.group(2)};",
        s,
        flags=re.I,
    )

    return s


def ensure_profile_history_teasers(s: str) -> str:
    # Convert any private/source controls first.
    s = make_history_range_teasers(s)

    if re.search(
        r'data-public-range=["\'](?:ALL|2000)["\']',
        s,
        flags=re.I,
    ):
        return s

    # Some profile templates may not currently contain the private controls.
    # In that case, add the two standard preview buttons before the public ones.
    pattern = re.compile(
        r'(<div\b[^>]*class=["\'][^"\']*\brange-buttons\b[^"\']*["\'][^>]*>)',
        flags=re.I,
    )

    match = pattern.search(s)
    if not match:
        return s

    teaser_html = (
        '\n            <button type="button" '
        'class="range-btn seg jr-public-disabled jr-public-history-teaser" '
        'data-public-range="ALL" disabled aria-disabled="true" '
        'title="Full history coming soon">All</button>'
        '\n            <button type="button" '
        'class="range-btn seg jr-public-disabled jr-public-history-teaser" '
        'data-public-range="2000" disabled aria-disabled="true" '
        'title="Full history coming soon">Since 2000</button>'
    )

    return s[:match.end()] + teaser_html + s[match.end():]


def ensure_profile_stats_teaser(s: str) -> str:
    # Remove the functional private Statistics link, if this sport already has it.
    s = re.sub(
        r'<a\b[^>]*\bid=["\']statsLink["\'][^>]*>[\s\S]*?</a>',
        (
            '<div class="stat jr-public-disabled jr-public-stats-teaser" '
            'aria-disabled="true" title="Advanced statistics coming soon">\n'
            '          <div class="stat-label">Statistics</div>\n'
            '          <div class="stat-value">Coming soon</div>\n'
            '        </div>'
        ),
        s,
        count=1,
        flags=re.I,
    )

    # Remove JavaScript which rewrites the now-non-functional private link.
    s = re.sub(
        r'\n\s*const statsLink = document\.getElementById\([\'"]statsLink[\'"]\);'
        r'\s*if \(statsLink && idRaw\) \{\s*'
        r'statsLink\.href = `\./stats/\?id=\$\{encodeURIComponent\(idRaw\)\}`;\s*\}',
        '\n',
        s,
        count=1,
        flags=re.I,
    )

    if "jr-public-stats-teaser" in s:
        return s

    # Other sports do not yet have a private Statistics page. Give them the
    # same public preview card immediately after Peak.
    peak_stat_re = re.compile(
        r'('
        r'<div\b[^>]*class=["\'][^"\']*\bstat\b[^"\']*["\'][^>]*>\s*'
        r'<div\b[^>]*class=["\'][^"\']*\bstat-label\b[^"\']*["\'][^>]*>\s*Peak\s*</div>\s*'
        r'<div\b[^>]*class=["\'][^"\']*\bstat-value\b[^"\']*["\'][^>]*'
        r'\bid=["\']peakVal["\'][^>]*>[\s\S]*?</div>\s*'
        r'</div>'
        r')',
        flags=re.I,
    )

    match = peak_stat_re.search(s)
    if not match:
        return s

    teaser = (
        '\n\n        <div class="stat jr-public-disabled jr-public-stats-teaser" '
        'aria-disabled="true" title="Advanced statistics coming soon">\n'
        '          <div class="stat-label">Statistics</div>\n'
        '          <div class="stat-value">Coming soon</div>\n'
        '        </div>'
    )

    return s[:match.end()] + teaser + s[match.end():]


def clean_european_football_public_ui() -> None:
    ef_dir = OUT_DIR / "EuropeanFootball"

    # European Football already has working private comparison/statistics
    # features. The public build removes the implementation/data routes but
    # leaves non-functional preview controls which are added by the generic
    # cleanup below.

    # -----------------------------
    # Home page
    # -----------------------------
    home = ef_dir / "home" / "index.html"
    if home.exists():
        s = home.read_text(encoding="utf-8")

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

        # Prevent private comparison setup from running.
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
        print("Cleaned public European Football home implementation")

    # -----------------------------
    # Peak page
    # -----------------------------
    peak = ef_dir / "peak" / "index.html"
    if peak.exists():
        s = peak.read_text(encoding="utf-8")

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

        # Peak filtering code also calls hideCompare(); keep it safe after the
        # comparison DOM has been removed.
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

        peak.write_text(s, encoding="utf-8", newline="\n")
        print("Cleaned public European Football peak implementation")

    # -----------------------------
    # Player page
    # -----------------------------
    player = ef_dir / "player" / "index.html"
    if player.exists():
        s = player.read_text(encoding="utf-8")

        # The source page contains JS that points Statistics at ./stats/.
        # Remove only that functional wiring; the generic cleanup will turn
        # the visible card into a disabled preview.
        s = re.sub(
            r'\n\s*const statsLink = document\.getElementById\([\'"]statsLink[\'"]\);'
            r'\s*if \(statsLink && idRaw\) \{\s*'
            r'statsLink\.href = `\./stats/\?id=\$\{encodeURIComponent\(idRaw\)\}`;\s*\}',
            '\n',
            s,
            count=1,
            flags=re.I,
        )

        player.write_text(s, encoding="utf-8", newline="\n")
        print("Prepared public European Football player implementation")


def assert_european_football_public_ui_clean() -> None:
    # Private implementation paths must remain absent even though preview
    # controls are now intentionally visible.
    checks = [
        (OUT_DIR / "EuropeanFootball" / "home" / "index.html", 'href="../compare/', 'home Compare link'),
        (OUT_DIR / "EuropeanFootball" / "peak" / "index.html", 'href="../compare/', 'peak Compare link'),
        (OUT_DIR / "EuropeanFootball" / "player" / "index.html", 'href="../compare/', 'player Compare link'),
        (OUT_DIR / "EuropeanFootball" / "player" / "index.html", 'href="./stats/', 'player Statistics link'),
    ]

    for path, needle, label in checks:
        if not path.exists():
            continue
        s = path.read_text(encoding="utf-8")
        if needle in s:
            raise RuntimeError(
                f"SAFETY CHECK FAILED: public UI still contains functional {label}: {path}"
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


def clean_generic_public_ui(sports: list[str] | None = None) -> None:
    sports = PUBLIC_SPORTS if sports is None else sports
    for sport in sports:
        sport_dir = OUT_DIR / sport
        if not sport_dir.exists():
            continue

        for html in sport_dir.rglob("*.html"):
            s = html.read_text(encoding="utf-8")

            # Public pages advertise the private feature set without linking to
            # it. The actual compare directories were already removed earlier.
            s = make_compare_teasers(s)

            # Turn pre-2010 historical controls into disabled previews rather
            # than deleting them from the page.
            s = make_history_range_teasers(s)

            # Prevent any existing private Compare launch code from attaching
            # to the disabled preview button.
            s = re.sub(
                r'\n\s*setupCompareButton\(\);\s*',
                '\n',
                s,
            )
            s = re.sub(
                r'\n\s*setupCompareChartControls\(\);\s*',
                '\n',
                s,
            )

            if "jr-public-disabled" in s:
                s = ensure_public_teaser_styles(s)

            html.write_text(s, encoding="utf-8", newline="\n")

        # All five player profiles advertise the same future Statistics feature
        # and the same locked full-history ranges, even where the private sport
        # has not implemented those advanced pages yet.
        player = sport_dir / "player" / "index.html"
        if player.exists():
            s = player.read_text(encoding="utf-8")
            s = ensure_profile_history_teasers(s)
            s = ensure_profile_stats_teaser(s)
            s = ensure_public_teaser_styles(s)
            player.write_text(s, encoding="utf-8", newline="\n")

    print("Generic public UI teaser cleanup applied across all sports")


def assert_generic_public_ui_clean(sports: list[str] | None = None) -> None:
    failures = []
    sports = PUBLIC_SPORTS if sports is None else sports

    for sport in sports:
        sport_dir = OUT_DIR / sport
        if not sport_dir.exists():
            continue

        if (sport_dir / "compare").exists():
            failures.append(f"{sport}: private compare directory still exists")

        for html in sport_dir.rglob("*.html"):
            s = html.read_text(encoding="utf-8")
            rel = html.relative_to(OUT_DIR).as_posix()

            # Teaser controls may say Compare / Since 2000 / All, but there
            # must be no functional route or live pre-2010 control behind them.
            if re.search(r'href=["\'][^"\']*compare/', s, flags=re.I):
                failures.append(f"{rel}: functional link to private compare section")

            if re.search(r'href=["\'][^"\']*(?:player/)?stats/', s, flags=re.I):
                failures.append(f"{rel}: functional link to private statistics section")

            if re.search(r'id=["\']compare-btn["\']', s, flags=re.I):
                failures.append(f"{rel}: functional Compare launch id remains")

            if re.search(r'\bsetupCompareButton\(\);', s):
                failures.append(f"{rel}: Compare-button initialiser remains")

            # Any pre-2010 value still carried by the live attributes would be
            # actionable by the graph JavaScript and therefore is not allowed.
            for match in re.finditer(
                r"<button\b[^>]*data-(range|since)=[\"']([^\"']*)[\"']",
                s,
                flags=re.I,
            ):
                kind = match.group(1).lower()
                value = match.group(2).strip()
                private = (
                    (kind == "range" and value.upper() == "ALL")
                    or (kind == "since" and value == "")
                    or (value.isdigit() and int(value) < 2010)
                )
                if private:
                    failures.append(
                        f"{rel}: active private historical control remains ({kind}={value or 'ALL'})"
                    )

        player = sport_dir / "player" / "index.html"
        if player.exists():
            s = player.read_text(encoding="utf-8")
            rel = player.relative_to(OUT_DIR).as_posix()

            if "jr-public-stats-teaser" not in s:
                failures.append(f"{rel}: Statistics coming-soon teaser missing")

            if not re.search(
                r'data-public-range=["\']ALL["\']',
                s,
                flags=re.I,
            ):
                failures.append(f"{rel}: All-history teaser missing")

            if not re.search(
                r'data-public-range=["\']2000["\']',
                s,
                flags=re.I,
            ):
                failures.append(f"{rel}: Since-2000 teaser missing")

        # Main pages should visibly advertise Compare without linking to it.
        for rel_page in ("home/index.html", "peak/index.html", "player/index.html"):
            page = sport_dir / rel_page
            if not page.exists():
                continue
            s = page.read_text(encoding="utf-8")
            if "jr-public-nav-teaser" not in s:
                failures.append(
                    f"{page.relative_to(OUT_DIR).as_posix()}: disabled Compare tab missing"
                )

    if failures:
        raise RuntimeError(
            "PUBLIC UI SAFETY CHECK FAILED:\n  " + "\n  ".join(failures)
        )

    print("Generic public-UI safety checks: PASS")


def _remove_tree(path: Path) -> None:
    if path.exists():
        shutil.rmtree(path)


def _choose_transient_previous(base: Path) -> Path:
    """Return an available transient previous-build path."""
    if not base.exists():
        return base

    try:
        _remove_tree(base)
        return base
    except OSError as exc:
        print(f"Warning: could not remove stale transient {base.name}: {exc}")

    index = 2
    while True:
        candidate = base.with_name(f"{base.name}_{index}")
        if not candidate.exists():
            return candidate
        index += 1


def _publish_verified_build() -> None:
    """Swap a fully verified temporary build into _public_site safely."""
    previous_dir = _choose_transient_previous(PREVIOUS_OUT_DIR)
    moved_live = False

    try:
        if LIVE_OUT_DIR.exists():
            LIVE_OUT_DIR.rename(previous_dir)
            moved_live = True

        TEMP_OUT_DIR.rename(LIVE_OUT_DIR)

    except Exception:
        if moved_live and previous_dir.exists() and not LIVE_OUT_DIR.exists():
            previous_dir.rename(LIVE_OUT_DIR)
        raise

    if previous_dir.exists():
        try:
            _remove_tree(previous_dir)
        except OSError as exc:
            print(f"Warning: could not remove transient {previous_dir.name}: {exc}")


def _publish_verified_sport(sport: str) -> None:
    """Swap only one verified sport directory into the existing public site."""
    temp_sport = TEMP_OUT_DIR / sport
    live_sport = LIVE_OUT_DIR / sport
    previous_base = REPO_DIR / f"_public_site_previous_{sport}"
    previous_dir = _choose_transient_previous(previous_base)

    if not temp_sport.exists():
        raise RuntimeError(f"Verified temporary sport build is missing: {temp_sport}")

    LIVE_OUT_DIR.mkdir(parents=True, exist_ok=True)
    moved_live = False

    try:
        if live_sport.exists():
            live_sport.rename(previous_dir)
            moved_live = True

        temp_sport.rename(live_sport)

    except Exception:
        if moved_live and previous_dir.exists() and not live_sport.exists():
            previous_dir.rename(live_sport)
        raise

    if previous_dir.exists():
        try:
            _remove_tree(previous_dir)
        except OSError as exc:
            print(f"Warning: could not remove transient {previous_dir.name}: {exc}")

    try:
        _remove_tree(TEMP_OUT_DIR)
    except OSError as exc:
        print(f"Warning: could not remove temporary {TEMP_OUT_DIR.name}: {exc}")


SPORT_ALIASES = {
    "europeanfootball": "EuropeanFootball",
    "european": "EuropeanFootball",
    "football": "EuropeanFootball",
    "go": "Go",
    "internationalfootball": "InternationalFootball",
    "international": "InternationalFootball",
    "rugbyunion": "RugbyUnion",
    "rugby": "RugbyUnion",
    "snooker": "Snooker",
}


def normalise_requested_sport(value: str) -> str:
    key = re.sub(r"[^a-z0-9]", "", str(value).lower())

    if key == "all":
        return "ALL"

    sport = SPORT_ALIASES.get(key)
    if sport:
        return sport

    valid = ", ".join(PUBLIC_SPORTS)
    raise SystemExit(
        f"Unknown sport: {value}\n"
        f"Use one of: {valid}\n"
        "Or run with no sport argument to rebuild everything."
    )


def _prepare_temp_dir() -> None:
    try:
        _remove_tree(TEMP_OUT_DIR)
    except OSError as exc:
        raise RuntimeError(
            f"Could not clear temporary build directory {TEMP_OUT_DIR}: {exc}\n"
            "Close any local process using _public_site_build and try again."
        ) from exc

    TEMP_OUT_DIR.mkdir(parents=True)


def _copy_full_public_source() -> None:
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


def _copy_one_sport_source(sport: str) -> None:
    src = REPO_DIR / sport
    dst = OUT_DIR / sport

    if not src.exists():
        raise RuntimeError(f"Sport source directory not found: {src}")

    copy_tree_filtered(src, dst)


def _clean_and_check_selected_sports(sports: list[str]) -> None:
    remove_private_paths()

    if "EuropeanFootball" in sports:
        assert_european_football_private_paths_absent()
        clean_european_football_public_ui()

    clean_generic_public_ui(sports)

    if "EuropeanFootball" in sports:
        assert_european_football_public_ui_clean()

    assert_generic_public_ui_clean(sports)

    for sport in sports:
        config = SPORT_PUBLIC_FILTERS[sport]
        filter_sport_public_data(sport, config)

    if "Snooker" in sports:
        filter_snooker_snapshots()

    for sport in sports:
        config = SPORT_PUBLIC_FILTERS[sport]
        assert_no_pre_2010_sport_data(sport, config)

    if "Snooker" in sports:
        assert_no_pre_2010_snooker_snapshots()


def _copy_existing_public_ui_from_source() -> int:
    """Overlay only HTML files whose exact paths already exist publicly."""
    if not LIVE_OUT_DIR.exists():
        raise RuntimeError(
            "UI-only build requires an existing _public_site. "
            "Run one full verified build first."
        )

    count = 0

    for filename in PUBLIC_ROOT_FILES:
        if not filename.lower().endswith(".html"):
            continue

        src = REPO_DIR / filename
        live = LIVE_OUT_DIR / filename
        dst = OUT_DIR / filename

        if src.exists() and live.exists():
            dst.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(src, dst)
            count += 1

    for dirname in PUBLIC_TOP_LEVEL_DIRS:
        src_root = REPO_DIR / dirname
        live_root = LIVE_OUT_DIR / dirname

        if not src_root.exists() or not live_root.exists():
            continue

        for src in src_root.rglob("*.html"):
            rel = src.relative_to(REPO_DIR)

            if should_ignore_dir(rel):
                continue

            live = LIVE_OUT_DIR / rel
            if not live.exists():
                continue

            dst = OUT_DIR / rel
            dst.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(src, dst)
            count += 1

    return count


def build_public_ui_only() -> None:
    """Fast verified rebuild for HTML/UI-only changes."""
    _prepare_temp_dir()

    if not LIVE_OUT_DIR.exists():
        raise RuntimeError(
            "Cannot run UI-only build because _public_site does not exist."
        )

    print("Copying existing verified public site...")
    shutil.copytree(LIVE_OUT_DIR, TEMP_OUT_DIR, dirs_exist_ok=True)

    updated = _copy_existing_public_ui_from_source()
    print(f"Public HTML files refreshed from source: {updated}")

    remove_private_paths()
    assert_european_football_private_paths_absent()
    clean_european_football_public_ui()
    clean_generic_public_ui()
    assert_european_football_public_ui_clean()
    assert_generic_public_ui_clean()

    # Data is inherited from the existing verified public site. Re-check it
    # without rewriting/re-filtering all historical JSON.
    for sport, config in SPORT_PUBLIC_FILTERS.items():
        assert_no_pre_2010_sport_data(sport, config)

    assert_no_pre_2010_snooker_snapshots()

    print()
    print(f"Verified temporary UI-only public build created at: {TEMP_OUT_DIR}")
    print("Historical public data was not recopied or re-filtered.")
    print("Public data and UI safety checks: PASS")

    _publish_verified_build()
    print(f"Published verified UI-only build to: {LIVE_OUT_DIR}")


def build_public_site(selected_sport: str | None = None) -> None:
    """Build the whole public site, or one sport when selected_sport is given."""
    _prepare_temp_dir()

    if selected_sport is None:
        sports = list(PUBLIC_SPORTS)
        _copy_full_public_source()
    else:
        sports = [selected_sport]
        _copy_one_sport_source(selected_sport)

    _clean_and_check_selected_sports(sports)

    print()
    if selected_sport is None:
        print(f"Verified temporary public build created at: {TEMP_OUT_DIR}")
        print("All configured sports have had detailed historical data filtered to 2010+.")
    else:
        print(f"Verified temporary {selected_sport} build created at: {TEMP_OUT_DIR / selected_sport}")
        print(f"{selected_sport} detailed historical data has been filtered to 2010+.")

    print("Public data and UI safety checks: PASS")

    if selected_sport is None:
        _publish_verified_build()
        print(f"Published verified build to: {LIVE_OUT_DIR}")
    else:
        _publish_verified_sport(selected_sport)
        print(f"Published verified sport build to: {LIVE_OUT_DIR / selected_sport}")


if __name__ == "__main__":
    args = sys.argv[1:]

    if args == ["--ui-only"]:
        build_public_ui_only()
    else:
        if len(args) > 1:
            raise SystemExit(
                "Usage:\n"
                "  python scripts\\public_site\\build_public_site.py\n"
                "  python scripts\\public_site\\build_public_site.py EuropeanFootball\n"
                "  python scripts\\public_site\\build_public_site.py --ui-only"
            )

        selected = None
        if args:
            selected = normalise_requested_sport(args[0])
            if selected == "ALL":
                selected = None

        build_public_site(selected)
