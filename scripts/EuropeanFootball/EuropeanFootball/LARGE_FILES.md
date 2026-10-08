# Large Club Football files

The local combined master CSV and end-of-2024 Elo checkpoint are ignored by Git.
Checked gzip parts under `EuropeanFootball/pipeline_data/versioned_inputs` retain both inputs.
The manifest records SHA-256 hashes for each part and complete restored file.

After a local update, before committing pipeline inputs:

    python scripts/EuropeanFootball/sync_large_pipeline_files.py pack

On a fresh checkout, before running R:

    python scripts/EuropeanFootball/sync_large_pipeline_files.py restore

Restore replaces local inputs with the committed versions; use it only deliberately.
Weekly automation restores before running and packs its changed inputs afterward.
The monthly review restores its inputs too.

Top Teams is exported to JSON parts plus a manifest under `EuropeanFootball/data/top_teams`.
The comparison page loads those parts. The old single JSON is ignored and no longer exported.
All other website JSON remains unchanged.
