from pathlib import Path
import re

ROOT = Path.cwd()

SKIP_DIRS = {
    ".git",
    "_public_site",
    "node_modules",
    ".venv",
    "venv",
    "__pycache__",
}

MARKER_START = "/* J-Ratings dark scrollbars: start */"
MARKER_END = "/* J-Ratings dark scrollbars: end */"

CSS = r"""
/* J-Ratings dark scrollbars: start */
html {
  scrollbar-color: #596273 #151b26;
  scrollbar-width: thin;
}

* {
  scrollbar-color: #596273 #151b26;
  scrollbar-width: thin;
}

*::-webkit-scrollbar {
  width: 10px;
  height: 10px;
}

*::-webkit-scrollbar-track {
  background: #151b26;
}

*::-webkit-scrollbar-thumb {
  background: #596273;
  border: 2px solid #151b26;
  border-radius: 8px;
}

*::-webkit-scrollbar-thumb:hover {
  background: #737d90;
}

*::-webkit-scrollbar-corner {
  background: #151b26;
}

/*
  Reserve scrollbar space where supported so scrollbars are less likely
  to sit over tables/content.
*/
html,
body,
[style*="overflow"],
[style*="overflow-y"],
[style*="overflow-x"] {
  scrollbar-gutter: stable;
}
/* J-Ratings dark scrollbars: end */
""".strip()

def should_skip(path: Path) -> bool:
    return any(part in SKIP_DIRS for part in path.parts)

def patch_html(path: Path):
    text = path.read_text(encoding="utf-8")

    # Already patched: replace the existing managed block so reruns stay safe.
    pattern = re.compile(
        re.escape(MARKER_START) + r".*?" + re.escape(MARKER_END),
        re.S,
    )
    if pattern.search(text):
        new_text = pattern.sub(CSS, text, count=1)
        status = "updated"
    elif "</style>" in text:
        new_text = text.replace("</style>", "\n" + CSS + "\n</style>", 1)
        status = "patched"
    elif "</head>" in text:
        new_text = text.replace(
            "</head>",
            "<style>\n" + CSS + "\n</style>\n</head>",
            1,
        )
        status = "patched"
    else:
        return "skipped-no-head"

    if new_text != text:
        path.write_text(new_text, encoding="utf-8")

    return status

html_files = sorted(
    p for p in ROOT.rglob("*.html")
    if not should_skip(p.relative_to(ROOT))
)

counts = {
    "patched": 0,
    "updated": 0,
    "skipped-no-head": 0,
    "unchanged": 0,
}

changed = []

for path in html_files:
    before = path.read_text(encoding="utf-8")
    result = patch_html(path)
    after = path.read_text(encoding="utf-8")

    if result == "skipped-no-head":
        counts[result] += 1
    elif before == after:
        counts["unchanged"] += 1
    else:
        counts[result] += 1
        changed.append(path.relative_to(ROOT))

print()
print("J-RATINGS SCROLLBAR UPDATE COMPLETE")
print(f"Repository: {ROOT}")
print(f"HTML files checked: {len(html_files)}")
print(f"Patched: {counts['patched']}")
print(f"Updated existing scrollbar block: {counts['updated']}")
print(f"Unchanged: {counts['unchanged']}")
print(f"Skipped (no </style> or </head>): {counts['skipped-no-head']}")

if changed:
    print()
    print("Changed files:")
    for path in changed:
        print(f"  {path}")

print()
print("Skipped directories:")
for name in sorted(SKIP_DIRS):
    print(f"  {name}")
