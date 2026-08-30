from __future__ import annotations

import argparse
import os
import re
import shutil
import stat
import sys
from datetime import datetime
from pathlib import Path


SCRIPT_DIR = Path(__file__).resolve().parent
REPO_DIR = SCRIPT_DIR.parent

BACKUP_ROOT = Path(r"C:\Users\stjuk\Documents\GitHub\J-Ratings (Backup)")
SOURCE_SPORT = "EuropeanFootball"

SPORTS = {
    "InternationalFootball": {
        "display": "International Football",
        "logo": "Football Banner Logo.png",
    },
    "RugbyUnion": {
        "display": "Rugby Union",
        "logo": "Rugby Banner Logo.png",
    },
    "Snooker": {
        "display": "Snooker",
        "logo": "Snooker Banner Logo.png",
    },
    "Go": {
        "display": "Go",
        "logo": "Go Banner Logo.png",
    },
}

# Advanced source areas to copy. Existing Home / Peak / Player pages are not
# wholesale-replaced: they contain sport-specific behaviour that should remain.
COPY_TREES = (
    ("compare", "compare"),
    ("playback", "playback"),
    ("simulate", "simulate"),
    ("player/stats", "player/stats"),
)


def parse_args():
    parser = argparse.ArgumentParser(
        description="Port European Football advanced private HTML features to other J-Ratings sports."
    )
    parser.add_argument(
        "targets",
        nargs="*",
        choices=sorted(SPORTS),
        help="Sports to update. Default: all four target sports.",
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Show what would change without writing anything.",
    )
    return parser.parse_args()


def ensure_repo_root():
    if not (REPO_DIR / ".git").exists():
        raise SystemExit(
            "ERROR: Expected this script at scripts\\port_advanced_features.py "
            "inside the J-Ratings repository."
        )

    if REPO_DIR.name != "J-Ratings":
        raise SystemExit(f"ERROR: Unexpected repository folder: {REPO_DIR}")

    public_dir = REPO_DIR / "_public_site"
    if SCRIPT_DIR == public_dir or public_dir in SCRIPT_DIR.parents:
        raise SystemExit("ERROR: Refusing to run from inside _public_site.")


def timestamped_backup_dir():
    stamp = datetime.now().strftime("%Y-%m-%d_%H%M%S")
    return BACKUP_ROOT / "advanced-feature-port" / stamp


def remove_tree(path: Path):
    """Remove a directory tree, clearing Windows read-only attributes if needed."""
    def handle_remove_error(func, name, exc_info):
        try:
            os.chmod(name, stat.S_IWRITE)
        except OSError:
            pass
        func(name)

    shutil.rmtree(path, onerror=handle_remove_error)


def backup_existing(path: Path, backup_run: Path, dry_run: bool):
    if not path.exists():
        return

    rel = path.relative_to(REPO_DIR)
    destination = backup_run / rel

    if dry_run:
        print(f"BACKUP   {rel} -> {destination}")
        return

    destination.parent.mkdir(parents=True, exist_ok=True)

    if path.is_dir():
        shutil.copytree(path, destination, dirs_exist_ok=False)
    else:
        shutil.copy2(path, destination)

    print(f"BACKUP   {rel}")


def read_target_data_names(target: str):
    """Discover registry/colour filenames from the target's existing player page."""
    player = REPO_DIR / target / "player" / "index.html"
    if not player.exists():
        raise RuntimeError(f"Missing target player page: {player}")

    text = player.read_text(encoding="utf-8")

    registry_match = re.search(
        r"""['"]\.\./data/([^/'"]+\.json)['"]""",
        text,
        flags=re.I,
    )
    if not registry_match:
        raise RuntimeError(
            f"{target}: could not discover the main registry JSON from player/index.html"
        )

    registry_name = registry_match.group(1)

    colour_match = re.search(
        r"""['"]\.\./([^/'"]*colou?r[^/'"]*\.json)['"]""",
        text,
        flags=re.I,
    )
    colour_name = colour_match.group(1) if colour_match else None

    return registry_name, colour_name


def transform_html(text: str, target: str, registry_name: str, colour_name: str | None):
    cfg = SPORTS[target]

    # Branding. Keep structural CSS/JS identifiers intact; only change human-facing
    # sport branding and logo file references.
    text = text.replace("European Football", cfg["display"])
    text = text.replace("EuropeanFootball", target)
    text = text.replace("Football Banner Logo.png", cfg["logo"])

    # The European advanced pages use teams.json. Other sports may use another
    # registry filename. Relative directory depth remains valid because the copied
    # tree preserves the source directory structure.
    if registry_name != "teams.json":
        text = text.replace("teams.json", registry_name)

    if colour_name and colour_name != "team-colours.json":
        text = text.replace("team-colours.json", colour_name)

    return text


def copy_and_transform_tree(
    source_rel: str,
    target_rel: str,
    target: str,
    registry_name: str,
    colour_name: str | None,
    backup_run: Path,
    dry_run: bool,
):
    source = REPO_DIR / SOURCE_SPORT / source_rel
    destination = REPO_DIR / target / target_rel

    if not source.exists():
        raise RuntimeError(f"Missing European Football source area: {source}")

    # One backup of the old destination before replacing it.
    backup_existing(destination, backup_run, dry_run)

    if dry_run:
        print(f"REPLACE  {destination.relative_to(REPO_DIR)} <- {source.relative_to(REPO_DIR)}")
        return

    if destination.exists():
        if destination.is_dir():
            remove_tree(destination)
        else:
            destination.unlink()

    for src in source.rglob("*"):
        rel = src.relative_to(source)
        dst = destination / rel

        # Never copy pipeline/source material as part of an HTML feature port.
        if "pipeline_data" in rel.parts or "_public_site" in rel.parts:
            continue

        if src.is_dir():
            dst.mkdir(parents=True, exist_ok=True)
            continue

        dst.parent.mkdir(parents=True, exist_ok=True)

        if src.suffix.lower() == ".html":
            text = src.read_text(encoding="utf-8")
            text = transform_html(
                text,
                target=target,
                registry_name=registry_name,
                colour_name=colour_name,
            )
            dst.write_text(text, encoding="utf-8", newline="\n")
        else:
            shutil.copy2(src, dst)

    print(f"REPLACED {destination.relative_to(REPO_DIR)}")


def ensure_stats_link(target: str, backup_run: Path, dry_run: bool):
    """Add the private Statistics card/link to the existing target profile page."""
    path = REPO_DIR / target / "player" / "index.html"
    text = path.read_text(encoding="utf-8")

    if 'id="statsLink"' in text:
        print(f"SKIP     {path.relative_to(REPO_DIR)} (Statistics link already present)")
        return

    # Locate the Peak stat card by its peakVal element and insert Statistics after it.
    peak_pattern = re.compile(
        r"""(
            <div\b[^>]*class=["'][^"']*\bstat\b[^"']*["'][^>]*>\s*
            <div\b[^>]*class=["'][^"']*\bstat-label\b[^"']*["'][^>]*>\s*Peak\s*</div>\s*
            <div\b[^>]*class=["'][^"']*\bstat-value\b[^"']*["'][^>]*\bid=["']peakVal["'][^>]*>
            [\s\S]*?</div>\s*
            </div>
        )""",
        flags=re.I | re.X,
    )

    match = peak_pattern.search(text)
    if not match:
        raise RuntimeError(f"{target}: could not locate the Peak profile stat card")

    card = (
        '\n\n        <a id="statsLink" class="stat stat-link" href="./stats/">\n'
        '          <div class="stat-label">Statistics</div>\n'
        '          <div class="stat-value">View</div>\n'
        '        </a>'
    )
    text = text[:match.end()] + card + text[match.end():]

    # Ensure the copied European stat-link CSS exists. This only adds the small
    # link styling; it does not replace the target profile page's own CSS.
    if ".team-stats .stat-link" not in text and ".player-stats .stat-link" not in text:
        css = """
    .team-stats .stat-link,
    .player-stats .stat-link{
      display:block;
      color:inherit;
      text-decoration:none;
      cursor:pointer;
      transition:background .15s ease, border-color .15s ease;
    }

    .team-stats .stat-link:hover,
    .player-stats .stat-link:hover{
      background:rgba(255,255,255,0.08);
      border-color:rgba(200,200,200,0.55);
    }
"""
        if "</style>" not in text:
            raise RuntimeError(f"{target}: player page has no </style> marker")
        text = text.replace("</style>", css + "\n  </style>", 1)

    # Wire ?id=... through to ./stats/.
    script_marker = "<script>"
    if script_marker not in text:
        raise RuntimeError(f"{target}: player page has no <script> marker")

    wiring = """
  const statsLink = document.getElementById('statsLink');
  if (statsLink && idRaw) {
    statsLink.href = `./stats/?id=${encodeURIComponent(idRaw)}`;
  }

"""

    # Insert after idRaw is declared rather than before it exists.
    id_match = re.search(
        r"""const\s+idRaw\s*=\s*params\.get\(['"]id['"]\);\s*""",
        text,
    )
    if not id_match:
        raise RuntimeError(f"{target}: could not locate idRaw declaration")

    text = text[:id_match.end()] + "\n\n" + wiring + text[id_match.end():]

    backup_existing(path, backup_run, dry_run)

    if dry_run:
        print(f"PATCH    {path.relative_to(REPO_DIR)} (add Statistics link)")
        return

    path.write_text(text, encoding="utf-8", newline="\n")
    print(f"PATCHED  {path.relative_to(REPO_DIR)}")


def validate_target(target: str):
    required = [
        REPO_DIR / target / "compare" / "index.html",
        REPO_DIR / target / "compare" / "manual" / "Compare Graphs" / "index.html",
        REPO_DIR / target / "compare" / "manual" / "Compare Profiles" / "index.html",
        REPO_DIR / target / "compare" / "top" / "index.html",
        REPO_DIR / target / "playback" / "index.html",
        REPO_DIR / target / "simulate" / "index.html",
        REPO_DIR / target / "player" / "stats" / "index.html",
        REPO_DIR / target / "player" / "index.html",
    ]

    missing = [p for p in required if not p.exists()]
    if missing:
        raise RuntimeError(
            f"{target}: validation failed; missing:\n  "
            + "\n  ".join(str(p.relative_to(REPO_DIR)) for p in missing)
        )

    profile = (REPO_DIR / target / "player" / "index.html").read_text(encoding="utf-8")
    if 'id="statsLink"' not in profile:
        raise RuntimeError(f"{target}: Statistics profile link was not installed")

    # Make sure branding was not accidentally left European in copied advanced pages.
    for path in required[:-1]:
        text = path.read_text(encoding="utf-8")
        if "European Football" in text or "EuropeanFootball" in text:
            raise RuntimeError(
                f"{target}: European Football branding remains in {path.relative_to(REPO_DIR)}"
            )

    print(f"VALIDATE {target}: PASS")


def main():
    args = parse_args()
    ensure_repo_root()

    targets = args.targets or list(SPORTS)
    backup_run = timestamped_backup_dir()

    print(f"Repository : {REPO_DIR}")
    print(f"Backup root: {backup_run}")
    print(f"Targets    : {', '.join(targets)}")
    print(f"Mode       : {'DRY RUN' if args.dry_run else 'WRITE'}")
    print()

    # Critical safety rule: this script works only on source-sport directories.
    for target in targets:
        target_dir = REPO_DIR / target
        if not target_dir.exists():
            raise RuntimeError(f"Missing target sport directory: {target_dir}")
        if "_public_site" in target_dir.parts:
            raise RuntimeError("Refusing to modify _public_site")

    for target in targets:
        print(f"==== {target} ====")
        registry_name, colour_name = read_target_data_names(target)

        print(f"Registry  : {registry_name}")
        print(f"Colours   : {colour_name or '(none detected)'}")

        for source_rel, target_rel in COPY_TREES:
            copy_and_transform_tree(
                source_rel=source_rel,
                target_rel=target_rel,
                target=target,
                registry_name=registry_name,
                colour_name=colour_name,
                backup_run=backup_run,
                dry_run=args.dry_run,
            )

        ensure_stats_link(target, backup_run, args.dry_run)

        if not args.dry_run:
            validate_target(target)

        print()

    if args.dry_run:
        print("DRY RUN COMPLETE - no files were changed.")
    else:
        print("ADVANCED FEATURE PORT: PASS")
        print(f"Backups: {backup_run}")
        print()
        print("Important:")
        print("  - Compare / Statistics should be tested against each sport's existing JSON.")
        print("  - Playback / Simulate are copied as private frameworks only.")
        print("  - Their tournament JSON still needs to be generated sport-by-sport.")
        print("  - _public_site was not modified.")


if __name__ == "__main__":
    try:
        main()
    except Exception as exc:
        print()
        print(f"ERROR: {exc}")
        sys.exit(1)
