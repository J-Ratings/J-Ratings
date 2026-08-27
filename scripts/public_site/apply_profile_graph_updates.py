
from pathlib import Path
import re
import shutil

ROOT = Path.cwd()

FILES = {
    "EuropeanFootball": ROOT / "EuropeanFootball" / "player" / "index.html",
    "InternationalFootball": ROOT / "InternationalFootball" / "player" / "index.html",
    "RugbyUnion": ROOT / "RugbyUnion" / "player" / "index.html",
    "Go": ROOT / "Go" / "player" / "index.html",
    "Snooker": ROOT / "Snooker" / "player" / "index.html",
}

DEFAULT_RANGE = {
    "EuropeanFootball": "2010",
    "InternationalFootball": "2010",
    "RugbyUnion": "2010",
    "Go": "2020",
    "Snooker": "2020",
}

DEFAULT_AGG = {
    "EuropeanFootball": "monthly",
    "InternationalFootball": "monthly",
    "RugbyUnion": "monthly",
    "Go": "weekly",
    "Snooker": "weekly",
}

AGG_LABELS = [
    ("daily", "Daily"),
    ("weekly", "Weekly"),
    ("monthly", "Monthly"),
    ("yearly", "Yearly"),
    ("decade", "Decade"),
]


def backup(path: Path) -> None:
    backup_path = path.with_suffix(path.suffix + ".before_graph_update.bak")
    if not backup_path.exists():
        shutil.copy2(path, backup_path)


def replace_one(pattern, repl, text, label, flags=0):
    out, n = re.subn(pattern, repl, text, count=1, flags=flags)
    if n != 1:
        raise RuntimeError(f"{label}: expected exactly one match, found {n}")
    return out


def agg_html(selected):
    rows = []
    for value, label in AGG_LABELS:
        mark = " selected" if value == selected else ""
        rows.append(f'              <option value="{value}"{mark}>{label}</option>')
    return '<select id="agg">\n' + "\n".join(rows) + "\n            </select>"


def range_html(default):
    def cls(key):
        return "range-btn seg is-active active" if key == default else "range-btn seg"

    return (
        '<div class="range-buttons" role="group" aria-label="Time range">\n'
        f'            <button type="button" class="{cls("ALL")}" data-range="ALL">All</button>\n'
        f'            <button type="button" class="{cls("2000")}" data-range="2000">Since 2000</button>\n'
        f'            <button type="button" class="{cls("2010")}" data-range="2010">Since 2010</button>\n'
        f'            <button type="button" class="{cls("2020")}" data-range="2020">Since 2020</button>\n'
        '          </div>'
    )


def patch_profile(sport, path):
    if not path.exists():
        raise FileNotFoundError(path)

    backup(path)
    s = path.read_text(encoding="utf-8")

    s = replace_one(
        r'<select id="agg">[\s\S]*?</select>',
        agg_html(DEFAULT_AGG[sport]),
        s,
        f"{sport}: aggregation menu",
    )

    s = replace_one(
        r'<div class="range-buttons"[^>]*>[\s\S]*?</div>',
        range_html(DEFAULT_RANGE[sport]),
        s,
        f"{sport}: range buttons",
    )

    s, n = re.subn(
        r"defaultRange:\s*['\"](?:ALL|\d{4})['\"]",
        f"defaultRange: '{DEFAULT_RANGE[sport]}'",
        s,
        count=1,
    )
    if n != 1:
        raise RuntimeError(f"{sport}: could not set defaultRange")

    if ".range-btn{" not in s and ".range-btn {" not in s:
        css = (
            "\n    .range-btn{\n"
            "      padding:8px 10px;\n"
            "      border:1px solid #2b2f36;\n"
            "      border-radius:10px;\n"
            "      background:rgba(255,255,255,0.03);\n"
            "      color:inherit;\n"
            "      cursor:pointer;\n"
            "      font-size:.95rem;\n"
            "    }\n\n"
            "    .range-btn:hover{ opacity:.92; }\n\n"
            "    .range-btn.active,\n"
            "    .range-btn.is-active{\n"
            "      background:rgba(255,255,255,0.10);\n"
            "      border-color:rgba(200,200,200,0.55);\n"
            "      font-weight:700;\n"
            "    }\n\n"
        )
        marker = re.search(r'(\s*\.player-stats\s*\{)', s)
        if not marker:
            raise RuntimeError(f"{sport}: could not insert standard range-button CSS")
        s = s[:marker.start()] + css + s[marker.start():]

    if "let ALL_TIME_PEAK = null;" not in s:
        s, n = re.subn(
            r'(let\s+(?:playerChart|teamChart)\s*=\s*null;\s*)',
            r'\1\n  let ALL_TIME_PEAK = null;\n',
            s,
            count=1,
        )
        if n != 1:
            raise RuntimeError(f"{sport}: could not add ALL_TIME_PEAK variable")

    if "me.peak_rating" not in s:
        peak_assign = (
            r'\1'
            "\n      ALL_TIME_PEAK = Number(\n"
            "        me.peak_rating ?? me.all_time_peak_rating\n"
            "      );\n"
        )
        s, n = re.subn(
            r'(if\s*\(!me\)\s*throw new Error\([^;]+;\s*)',
            peak_assign,
            s,
            count=1,
        )
        if n != 1:
            raise RuntimeError(f"{sport}: could not add registry peak assignment")

    team_old = "peakEl.textContent = peak ? String(Math.round(peak.y)) : '—';"
    team_new = (
        "const displayPeak = Number.isFinite(ALL_TIME_PEAK)\n"
        "      ? ALL_TIME_PEAK\n"
        "      : (peak ? peak.y : NaN);\n"
        "    peakEl.textContent = Number.isFinite(displayPeak)\n"
        "      ? String(Math.round(displayPeak))\n"
        "      : '—';"
    )
    s = s.replace(team_old, team_new, 1)

    player_old = "peakEl.textContent = Number.isFinite(peak.y) ? String(Math.round(peak.y)) : '—';"
    player_new = (
        "const displayPeak = Number.isFinite(ALL_TIME_PEAK) ? ALL_TIME_PEAK : peak.y;\n"
        "    peakEl.textContent = Number.isFinite(displayPeak)\n"
        "      ? String(Math.round(displayPeak))\n"
        "      : '—';"
    )
    s = s.replace(player_old, player_new, 1)

    multiline_pattern = (
        r"peakEl\.textContent = Number\.isFinite\(peak\.y\)\s*"
        r"\? String\(Math\.round\(peak\.y\)\)\s*"
        r": '—';"
    )
    s = re.sub(multiline_pattern, player_new, s, count=1)

    path.write_text(s, encoding="utf-8", newline="\n")
    print(f"Updated: {path.relative_to(ROOT)}")


def patch_public_builder():
    path = ROOT / "scripts" / "public_site" / "build_public_site.py"
    if not path.exists():
        raise FileNotFoundError(path)

    backup(path)
    s = path.read_text(encoding="utf-8")

    old = (
        '            # Public detailed history begins in 2010.\n'
        '            s = s.replace(\'data-range="2000"\', \'data-range="2010"\')\n'
        '            s = s.replace("data-range=\'2000\'", "data-range=\'2010\'")\n'
        '            s = s.replace(\'data-since="2000"\', \'data-since="2010"\')\n'
        '            s = s.replace("data-since=\'2000\'", "data-since=\'2010\'")\n'
        '            s = s.replace("Since 2000", "Since 2010")\n'
        '            s = s.replace("defaultRange: \'2000\'", "defaultRange: \'2010\'")\n'
        '            s = s.replace(\'defaultRange: "2000"\', \'defaultRange: "2010"\')\n'
    )

    new = (
        '            # Public detailed history begins in 2010. Keep each sport\\\'s\n'
        '            # intended default (2010 for ball sports, 2020 for Go/Snooker),\n'
        '            # but remove private historical presets.\n'
        '            s = re.sub(\n'
        '                r\'\\\\s*<button\\\\b[^>]*data-range=["\\\\\\\'](?:ALL|2000)["\\\\\\\'][^>]*>[\\\\s\\\\S]*?</button>\\\\s*\',\n'
        '                "\\\\n",\n'
        '                s,\n'
        '                flags=re.I,\n'
        '            )\n'
    )

    if old in s:
        s = s.replace(old, new, 1)
    elif "private historical presets" not in s:
        raise RuntimeError(
            "build_public_site.py: expected public historical-range cleanup block not found"
        )

    safety_old = (
        '            if (\n'
        '                \'data-range="2000"\' in s\n'
        '                or "data-range=\'2000\'" in s\n'
        '                or \'data-since="2000"\' in s\n'
        '                or "data-since=\'2000\'" in s\n'
        '                or "Since 2000" in s\n'
        '                or "defaultRange: \'2000\'" in s\n'
        '                or \'defaultRange: "2000"\' in s\n'
        '            ):\n'
        '                failures.append(f"{rel}: pre-2010 UI range remains")\n'
    )

    safety_new = (
        '            if re.search(\n'
        '                r\'<button\\\\b[^>]*data-range=["\\\\\\\'](?:ALL|2000)["\\\\\\\']\',\n'
        '                s,\n'
        '                flags=re.I,\n'
        '            ):\n'
        '                failures.append(f"{rel}: private historical preset remains")\n\n'
        '            if (\n'
        '                \'data-since="2000"\' in s\n'
        '                or "data-since=\'2000\'" in s\n'
        '                or "Since 2000" in s\n'
        '            ):\n'
        '                failures.append(f"{rel}: pre-2010 UI range remains")\n'
    )

    if safety_old in s:
        s = s.replace(safety_old, safety_new, 1)

    path.write_text(s, encoding="utf-8", newline="\n")
    print(f"Updated: {path.relative_to(ROOT)}")


def main():
    if not (ROOT / ".git").exists():
        raise SystemExit(
            "Run this from the J-Ratings repository root (the folder containing .git)."
        )

    for sport, path in FILES.items():
        patch_profile(sport, path)

    patch_public_builder()

    print()
    print("Profile graph-control patch: PASS")
    print("Next: replace root jratings-graph.js with the supplied updated JS file.")
    print(r"Then run: python scripts\public_site\build_public_site.py")


if __name__ == "__main__":
    main()
