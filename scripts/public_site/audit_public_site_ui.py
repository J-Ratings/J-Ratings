from pathlib import Path
from urllib.parse import urlsplit, unquote
import re

ROOT = Path("_public_site").resolve()

if not ROOT.exists():
    raise SystemExit("ERROR: _public_site does not exist. Run the public-site build first.")

HTML_FILES = sorted(ROOT.rglob("*.html"))

broken_links = []
active_private_controls = []
functional_private_links = []
missing_teasers = []

PUBLIC_SPORTS = [
    "EuropeanFootball",
    "Go",
    "InternationalFootball",
    "RugbyUnion",
    "Snooker",
]

IGNORE_PREFIXES = (
    "http://",
    "https://",
    "mailto:",
    "tel:",
    "javascript:",
    "data:",
)


def resolve_local_target(html_path: Path, href: str):
    href = href.strip()

    if not href or href.startswith("#") or href.lower().startswith(IGNORE_PREFIXES):
        return None

    parsed = urlsplit(href)
    path_text = unquote(parsed.path)

    if not path_text:
        return None

    if path_text.startswith("/"):
        target = ROOT / path_text.lstrip("/")
    else:
        target = html_path.parent / path_text

    target = target.resolve()

    try:
        target.relative_to(ROOT)
    except ValueError:
        return None

    return target


def target_exists(target: Path) -> bool:
    if target.exists():
        if target.is_dir():
            return (target / "index.html").exists()
        return True

    if target.suffix == "":
        if target.with_suffix(".html").exists():
            return True
        if (target / "index.html").exists():
            return True

    return False


href_re = re.compile(r'href\s*=\s*["\']([^"\']+)["\']', re.I)

# Only ACTIVE graph controls are a problem now. Disabled teaser controls use
# data-public-range and are intentionally allowed to say "All" / "Since 2000".
range_button_re = re.compile(
    r'<button\b[^>]*data-(range|since)=["\']([^"\']*)["\']',
    re.I,
)

private_href_re = re.compile(
    r'href\s*=\s*["\']([^"\']*(?:compare/|player/stats/|/stats/)[^"\']*)["\']',
    re.I,
)

for html in HTML_FILES:
    text = html.read_text(encoding="utf-8", errors="replace")
    rel = html.relative_to(ROOT).as_posix()

    for href in href_re.findall(text):
        target = resolve_local_target(html, href)
        if target is None:
            continue

        if not target_exists(target):
            broken_links.append((rel, href))

    for match in range_button_re.finditer(text):
        kind = match.group(1).lower()
        value = match.group(2).strip()

        private = (
            (kind == "range" and value.upper() == "ALL")
            or (kind == "since" and value == "")
            or (value.isdigit() and int(value) < 2010)
        )

        if private:
            line_no = text.count("\n", 0, match.start()) + 1
            active_private_controls.append(
                (rel, line_no, kind, value or "ALL")
            )

    for match in private_href_re.finditer(text):
        line_no = text.count("\n", 0, match.start()) + 1
        functional_private_links.append(
            (rel, line_no, match.group(1))
        )


# Main public pages should advertise Compare as a disabled preview, and all
# player profiles should advertise Statistics + full-history previews.
for sport in PUBLIC_SPORTS:
    sport_dir = ROOT / sport
    if not sport_dir.exists():
        continue

    for rel_page in ("home/index.html", "peak/index.html", "player/index.html"):
        page = sport_dir / rel_page
        if not page.exists():
            continue

        text = page.read_text(encoding="utf-8", errors="replace")
        if "jr-public-nav-teaser" not in text:
            missing_teasers.append(
                f"{page.relative_to(ROOT).as_posix()}: disabled Compare tab missing"
            )

    player = sport_dir / "player" / "index.html"
    if player.exists():
        text = player.read_text(encoding="utf-8", errors="replace")
        rel = player.relative_to(ROOT).as_posix()

        if "jr-public-stats-teaser" not in text:
            missing_teasers.append(f"{rel}: Statistics teaser missing")

        if not re.search(r'data-public-range=["\']ALL["\']', text, re.I):
            missing_teasers.append(f"{rel}: All-history teaser missing")

        if not re.search(r'data-public-range=["\']2000["\']', text, re.I):
            missing_teasers.append(f"{rel}: Since-2000 teaser missing")


print(f"HTML files checked: {len(HTML_FILES)}")
print()

if broken_links:
    print("BROKEN LOCAL LINKS:")
    for page, href in broken_links:
        print(f"  {page}  ->  {href}")
else:
    print("Broken local links: PASS")

print()

if active_private_controls:
    print("ACTIVE PRE-2010 CONTROLS:")
    for page, line_no, kind, value in active_private_controls:
        print(f"  {page}:{line_no}  ->  data-{kind}={value}")
else:
    print("Active pre-2010 controls: PASS")

print()

if functional_private_links:
    print("FUNCTIONAL PRIVATE LINKS:")
    for page, line_no, href in functional_private_links:
        print(f"  {page}:{line_no}  ->  {href}")
else:
    print("Functional private links: PASS")

print()

if missing_teasers:
    print("MISSING PUBLIC TEASERS:")
    for item in missing_teasers:
        print(f"  {item}")
else:
    print("Public teaser controls: PASS")

print()

if broken_links or active_private_controls or functional_private_links or missing_teasers:
    print("AUDIT RESULT: REVIEW NEEDED")
else:
    print("AUDIT RESULT: PASS")
