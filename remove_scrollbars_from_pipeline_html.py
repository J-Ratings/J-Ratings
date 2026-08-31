from pathlib import Path
import re

ROOT = Path.cwd()

MARKER_START = "/* J-Ratings dark scrollbars: start */"
MARKER_END = "/* J-Ratings dark scrollbars: end */"

pattern = re.compile(
    r"\s*" + re.escape(MARKER_START) + r".*?" + re.escape(MARKER_END) + r"\s*",
    re.S,
)

cleaned = []

for path in ROOT.rglob("*.html"):
    rel = path.relative_to(ROOT)

    # Remove the site scrollbar block from pipeline/source/audit HTML only.
    if "pipeline_data" not in {part.lower() for part in rel.parts}:
        continue

    text = path.read_text(encoding="utf-8")
    new_text, n = pattern.subn("\n", text)

    if n:
        path.write_text(new_text, encoding="utf-8")
        cleaned.append(rel)

print()
print("J-RATINGS PIPELINE HTML CLEANUP COMPLETE")
print(f"Repository: {ROOT}")
print(f"Pipeline HTML files cleaned: {len(cleaned)}")

if cleaned:
    print()
    print("Cleaned files:")
    for path in cleaned:
        print(f"  {path}")

print()
print("The real site HTML files were left unchanged.")
