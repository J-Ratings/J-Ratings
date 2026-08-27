from pathlib import Path

path = Path("scripts/public_site/build_public_site.py")
text = path.read_text(encoding="utf-8")

tag = "Final public cleanup of legacy peak-page pre-2010 controls"

if tag in text:
    print("LATEST PATCH CURRENT")
    print("Legacy peak-page public cleanup is already present.")
    print("PASS")
else:
    marker = '    print("Generic public UI cleanup applied across all sports")\n'
    if marker not in text:
        raise RuntimeError(
            "Could not find the generic public UI cleanup marker. No changes were made."
        )

    block = (
        '    # Final public cleanup of legacy peak-page pre-2010 controls.\n'
        '    # Private/source pages keep their older-history controls; public output does not.\n'
        '    for rel in (\n'
        '        Path("InternationalFootball/peak/index.html"),\n'
        '        Path("RugbyUnion/peak/index.html"),\n'
        '    ):\n'
        '        html_path = OUT_DIR / rel\n'
        '        if not html_path.exists():\n'
        '            continue\n'
        '        s = html_path.read_text(encoding="utf-8")\n'
        '        s = re.sub(\n'
        '            r\'\\s*<button\\b[^>]*data-since=["\\\']2000["\\\'][^>]*>[\\s\\S]*?</button>\\s*\',\n'
        '            "\\n",\n'
        '            s,\n'
        '            flags=re.I,\n'
        '        )\n'
        '        s = s.replace("let COMPARE_SINCE_YEAR = 2000;", "let COMPARE_SINCE_YEAR = 2010;")\n'
        '        s = s.replace("COMPARE_SINCE_YEAR = 2000;", "COMPARE_SINCE_YEAR = 2010;")\n'
        '        s = s.replace(\'.range-btn[data-since="2000"]\', \'.range-btn[data-since="2010"]\')\n'
        '        s = s.replace("since2000Btn", "since2010Btn")\n'
        '        html_path.write_text(s, encoding="utf-8")\n\n'
    )

    text = text.replace(marker, block + marker, 1)
    path.write_text(text, encoding="utf-8", newline="\n")

    print("LATEST PATCH CURRENT")
    print("Added public cleanup for International Football / Rugby Union peak-page 2000 controls.")
    print("PASS")