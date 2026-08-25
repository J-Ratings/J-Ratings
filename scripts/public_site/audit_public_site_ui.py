from pathlib import Path
from urllib.parse import urlsplit, unquote
import re

ROOT = Path("_public_site").resolve()

if not ROOT.exists():
    raise SystemExit("ERROR: _public_site does not exist. Run the public-site build first.")

HTML_FILES = sorted(ROOT.rglob("*.html"))

broken_links = []
old_range_refs = []

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


href_re = re.compile(r"href\s*=\s*[\"']([^\"']+)[\"']", re.I)
range_re = re.compile(
    r"(?:Since\s+|data-(?:range|since)\s*=\s*[\"'])(19\d{2}|200\d)(?:[\"']|\b)",
    re.I,
)

for html in HTML_FILES:
    text = html.read_text(encoding="utf-8", errors="replace")

    for href in href_re.findall(text):
        target = resolve_local_target(html, href)
        if target is None:
            continue

        if not target_exists(target):
            broken_links.append(
                (html.relative_to(ROOT).as_posix(), href)
            )

    for match in range_re.finditer(text):
        year = int(match.group(1))
        if year < 2010:
            line_no = text.count("\n", 0, match.start()) + 1
            old_range_refs.append(
                (html.relative_to(ROOT).as_posix(), line_no, year)
            )


print(f"HTML files checked: {len(HTML_FILES)}")
print()

if broken_links:
    print("BROKEN LOCAL LINKS:")
    for page, href in broken_links:
        print(f"  {page}  ->  {href}")
else:
    print("Broken local links: PASS")

print()

if old_range_refs:
    print("PRE-2010 UI RANGE REFERENCES:")
    for page, line_no, year in old_range_refs:
        print(f"  {page}:{line_no}  ->  {year}")
else:
    print("Pre-2010 UI range references: PASS")

print()
if broken_links or old_range_refs:
    print("AUDIT RESULT: REVIEW NEEDED")
else:
    print("AUDIT RESULT: PASS")
