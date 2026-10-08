"""Append the selected, dated UEFA candidates to the production match CSV."""
import csv, os, shutil
from datetime import datetime

root = r"C:\Users\stjuk\Documents\GitHub\J-Ratings"
master = os.path.join(root, "EuropeanFootball", "pipeline_data", "Matches_Clean_Combined", "european_football_all_matches.csv")
candidates = os.path.join(root, "EuropeanFootball", "pipeline_data", "Manual_Sources", "Wikipedia_RSSSF_Remaining_UEFA_Leagues", "dated_candidate.csv")
selected = {"Bosnia and Herzegovina": 2012, "Bulgaria": 2012, "Hungary": 2012, "Iceland": 2013,
            "Israel": 2012, "Luxembourg": 2012, "Malta": 2012, "San Marino": 2012, "Serbia": 2012}
backup = master.replace(".csv", "_before_selected_uefa_" + datetime.now().strftime("%Y%m%d_%H%M%S") + ".csv")
shutil.copy2(master, backup)

with open(candidates, newline="", encoding="utf-8-sig") as f:
    source = list(csv.DictReader(f))
source = [r for r in source if r["Country"] in selected and int(r["Season"][:4]) >= selected[r["Country"]] and int(r["Season"][:4]) <= 2024 and r["DateStatus"] == "matched"]

with open(master, newline="", encoding="utf-8-sig") as f:
    old = list(csv.DictReader(f))
fields = list(old[0])
keys = {(r.get("Date", ""), r.get("Home", ""), r.get("Away", ""), r.get("Score", ""), r.get("Country", "")) for r in old}
for r in source:
    row = {k: "" for k in fields}
    row.update({"Season": r["Season"], "Country": r["Country"], "Competition": r["Competition"], "CompetitionType": "league", "Tier": "1", "League": r["League"], "Date": r["Date"], "Home": r["Home"], "Away": r["Away"], "Result": r["Result"], "Score": r["Score"], "Source": r["Source"], "SourcePage": r["SourcePage"], "Stage": r["Stage"], "DateApprox": "FALSE", "SourceFile": r["DateSourcePage"]})
    key = (row["Date"], row["Home"], row["Away"], row["Score"], row["Country"])
    if key not in keys:
        old.append(row); keys.add(key)
with open(master, "w", newline="", encoding="utf-8") as f:
    writer = csv.DictWriter(f, fieldnames=fields); writer.writeheader(); writer.writerows(old)
print("Added", len(source), "dated candidate rows; master now has", len(old), "rows")
print("Backup:", backup)
