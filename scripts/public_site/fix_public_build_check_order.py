
from pathlib import Path
import re

path = Path("scripts/public_site/build_public_site.py")
text = path.read_text(encoding="utf-8")

pattern = re.compile(
    r"(?m)^(\s*)clean_european_football_public_ui\(\)\s*\n"
    r"\1assert_european_football_public_ui_clean\(\)\s*\n"
    r"\1clean_generic_public_ui\(\)\s*\n"
    r"\1assert_generic_public_ui_clean\(\)\s*$"
)

match = pattern.search(text)
if not match:
    raise RuntimeError(
        "Could not find the expected four-line cleanup/check block. No changes were made."
    )

indent = match.group(1)
replacement = (
    f"{indent}clean_european_football_public_ui()\n"
    f"{indent}clean_generic_public_ui()\n"
    f"{indent}assert_european_football_public_ui_clean()\n"
    f"{indent}assert_generic_public_ui_clean()"
)

text = text[:match.start()] + replacement + text[match.end():]
path.write_text(text, encoding="utf-8", newline="\n")

print("Fixed public build order.")
print("New order:")
print("  clean_european_football_public_ui()")
print("  clean_generic_public_ui()")
print("  assert_european_football_public_ui_clean()")
print("  assert_generic_public_ui_clean()")
print("PASS")
