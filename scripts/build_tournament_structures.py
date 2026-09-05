from __future__ import annotations

import argparse
import json
from collections import defaultdict
from pathlib import Path
from typing import Any


# ---------------------------------------------------------------------------
# Repository / JSON helpers
# ---------------------------------------------------------------------------

def repo_root() -> Path:
    """
    Expected location:
        <repo>/scripts/build_tournament_structures.py

    Therefore parents[1] is the repository root.
    """
    return Path(__file__).resolve().parents[1]


def load_json(path: Path) -> Any:
    return json.loads(path.read_text(encoding="utf-8"))


def save_json(path: Path, obj: Any) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(
        json.dumps(obj, ensure_ascii=False, separators=(",", ":")) + "\n",
        encoding="utf-8",
    )


# ---------------------------------------------------------------------------
# Match / component helpers
# ---------------------------------------------------------------------------

def component_sets(matches: list[dict[str, Any]]):
    """
    Detect disconnected sets of teams from a block of matches.

    This is useful for reconstructing group membership from result-only JSON.
    """
    adj: dict[str, set[str]] = defaultdict(set)
    first_seen: dict[str, int] = {}

    for idx, match in enumerate(matches):
        home = match["home"]
        away = match["away"]
        adj[home].add(away)
        adj[away].add(home)
        first_seen.setdefault(home, idx)
        first_seen.setdefault(away, idx)

    seen: set[str] = set()
    components: list[set[str]] = []

    for team in adj:
        if team in seen:
            continue

        stack = [team]
        component: set[str] = set()

        while stack:
            current = stack.pop()
            if current in seen:
                continue

            seen.add(current)
            component.add(current)
            stack.extend(adj[current] - seen)

        components.append(component)

    components.sort(key=lambda comp: min(first_seen[t] for t in comp))
    return components, first_seen


def component_sizes(matches: list[dict[str, Any]]) -> list[int]:
    components, _ = component_sets(matches)
    return sorted(len(component) for component in components)


# ---------------------------------------------------------------------------
# Template matching
# ---------------------------------------------------------------------------

def template_matches(template: dict[str, Any], data: dict[str, Any]) -> bool:
    signature = template.get("signature", {})

    expected_teams = signature.get("teams")
    expected_matches = signature.get("matches")

    if expected_teams is not None and len(data.get("teams", [])) != int(expected_teams):
        return False

    if expected_matches is not None:
        actual_matches = len(data.get("matches", []))
        expected_matches = int(expected_matches)

        # A format may explicitly allow a partially published fixture list.
        # This is useful for live league seasons. Historical completeness is
        # still validated by the sport's data writer before structures are built.
        if signature.get("allowIncompleteMatches", False):
            if actual_matches > expected_matches:
                return False
        elif actual_matches != expected_matches:
            return False

    detect = template.get("detect")
    if not detect:
        return True

    start = int(detect["afterMatch"])
    count = int(detect["probeMatchCount"])
    probe = data["matches"][start:start + count]

    return component_sizes(probe) == sorted(int(x) for x in detect["componentSizes"])


def choose_template(
    templates: list[dict[str, Any]],
    data: dict[str, Any],
) -> dict[str, Any]:
    matches = [template for template in templates if template_matches(template, data)]

    if len(matches) != 1:
        ids = [template.get("id", "<unnamed>") for template in matches]
        raise RuntimeError(
            f"{data.get('editionId')}: expected exactly one format template; "
            f"matched {ids or 'none'}"
        )

    return matches[0]


# ---------------------------------------------------------------------------
# Group-stage scoring / ordering
# ---------------------------------------------------------------------------

def _resolved_points_value(value: Any, edition_id: str, stage: dict[str, Any]) -> int:
    """
    Resolve an explicit numeric value or a template-defined edition switch.

    Backwards compatibility:
      International Football templates already use:
          "win": "edition"
      for the 24-team World Cup era. That means:
          before 1994 -> 2
          1994 onward -> 3

    New templates should prefer an explicit "pointsByEdition" mapping instead.
    """
    if value != "edition":
        return int(value)

    points_by_edition = stage.get("pointsByEdition")
    if points_by_edition:
        year = int(edition_id)
        selected = None

        for rule in points_by_edition:
            from_year = int(rule.get("from", -10**9))
            to_year = int(rule.get("to", 10**9))
            if from_year <= year <= to_year:
                selected = rule
                break

        if selected is None:
            raise RuntimeError(
                f"{edition_id}: no pointsByEdition rule applies to stage "
                f"{stage.get('label', '<unnamed>')}"
            )

        return int(selected["win"])

    # Legacy International Football behaviour.
    return 3 if int(edition_id) >= 1994 else 2


def actual_points(stage: dict[str, Any], edition_id: str) -> dict[str, int]:
    raw = stage.get("points", {"win": 3, "draw": 1, "loss": 0})

    return {
        "win": _resolved_points_value(raw.get("win", 3), edition_id, stage),
        "draw": int(raw.get("draw", 1)),
        "loss": int(raw.get("loss", 0)),
    }


def order_components(
    stage: dict[str, Any],
    components: list[set[str]],
    first_seen: dict[str, int],
) -> list[set[str]]:
    anchors = stage.get("componentOrderAnchors")

    if anchors:
        ordered: list[set[str]] = []
        unused = list(components)

        for anchor in anchors:
            found = next((component for component in unused if anchor in component), None)

            if found is None:
                raise RuntimeError(
                    f"Could not find component containing anchor {anchor!r}"
                )

            ordered.append(found)
            unused.remove(found)

        ordered.extend(
            sorted(unused, key=lambda comp: min(first_seen[t] for t in comp))
        )
        return ordered

    return sorted(
        components,
        key=lambda comp: min(first_seen[t] for t in comp),
    )


def stage_groups(
    stage: dict[str, Any],
    raw_matches: list[dict[str, Any]],
    label_anchors: dict[str, str] | None = None,
) -> dict[str, list[str]]:
    components, first_seen = component_sets(raw_matches)

    if label_anchors:
        by_label: dict[str, set[str]] = {}
        unused = list(components)

        for label in stage["groupLabels"]:
            anchor = label_anchors.get(label)

            if anchor is None:
                raise RuntimeError(
                    f"{stage['label']}: no anchor supplied for group {label}"
                )

            found = next(
                (component for component in unused if anchor in component),
                None,
            )

            if found is None:
                raise RuntimeError(
                    f"{stage['label']}: anchor {anchor!r} not found in any group"
                )

            by_label[label] = found
            unused.remove(found)

        components = [by_label[label] for label in stage["groupLabels"]]

    else:
        components = order_components(stage, components, first_seen)

    expected_sizes = sorted(int(size) for size in stage["groupSizes"])
    got_sizes = sorted(len(component) for component in components)

    if got_sizes != expected_sizes:
        raise RuntimeError(
            f"{stage['label']}: expected group sizes {expected_sizes}, "
            f"detected {got_sizes}"
        )

    labels = stage["groupLabels"]

    if len(labels) != len(components):
        raise RuntimeError(
            f"{stage['label']}: {len(labels)} labels for "
            f"{len(components)} groups"
        )

    groups: dict[str, list[str]] = {}

    for label, component in zip(labels, components):
        groups[label] = sorted(component, key=lambda team: first_seen[team])

    return groups


def _match_score(match: dict[str, Any], side: str) -> int:
    key = f"{side}Score"

    if key not in match:
        raise RuntimeError(f"Match is missing required score field {key!r}: {match}")

    return int(match[key])


def _match_tries(match: dict[str, Any], side: str) -> int | None:
    """
    Optional Rugby Union support.

    Accepted keys:
        homeTries / awayTries
        home_tries / away_tries
        homeTryCount / awayTryCount

    If try counts are absent, try-bonus rules cannot be calculated automatically.
    """
    candidates = (
        f"{side}Tries",
        f"{side}_tries",
        f"{side}TryCount",
    )

    for key in candidates:
        if key in match and match[key] is not None:
            return int(match[key])

    return None


def _bonus_points_for_match(
    match: dict[str, Any],
    home_score: int,
    away_score: int,
    stage: dict[str, Any],
) -> tuple[int, int]:
    """
    Generic Rugby Union-style bonus-point support.

    A stage may define:

        "bonusPoints": {
          "tryBonus": {
            "tries": 4,
            "points": 1
          },
          "losingBonus": {
            "maxMargin": 7,
            "points": 1
          }
        }

    Try bonus requires try counts in the source match JSON.
    Losing bonus only requires the scores.

    If no bonusPoints block exists, returns (0, 0).
    """
    rules = stage.get("bonusPoints")
    if not rules:
        return 0, 0

    home_bonus = 0
    away_bonus = 0

    try_rule = rules.get("tryBonus")
    if try_rule:
        threshold = int(try_rule["tries"])
        points = int(try_rule.get("points", 1))
        home_tries = _match_tries(match, "home")
        away_tries = _match_tries(match, "away")

        if home_tries is None or away_tries is None:
            raise RuntimeError(
                "Template requires try-bonus calculation, but the source match "
                "JSON does not contain try counts."
            )

        if home_tries >= threshold:
            home_bonus += points
        if away_tries >= threshold:
            away_bonus += points

    losing_rule = rules.get("losingBonus")
    if losing_rule and home_score != away_score:
        max_margin = int(losing_rule["maxMargin"])
        points = int(losing_rule.get("points", 1))
        margin = abs(home_score - away_score)

        if margin <= max_margin:
            if home_score < away_score:
                home_bonus += points
            else:
                away_bonus += points

    return home_bonus, away_bonus


def standings(
    rows_matches: list[dict[str, Any]],
    team_ids: list[str],
    points: dict[str, int],
    stage: dict[str, Any],
):
    """
    Sport-neutral standings core.

    Internally uses score-for / score-against / score-difference. For football
    those are goals; for rugby they are points. Legacy gf/ga/gd aliases are kept
    so existing International Football behaviour is unchanged.
    """
    team_ids = list(team_ids)

    rows = {
        team: {
            "id": team,
            "p": 0,
            "w": 0,
            "d": 0,
            "l": 0,
            "scoreFor": 0,
            "scoreAgainst": 0,
            "scoreDifference": 0,
            "bonus": 0,
            "pts": 0,
        }
        for team in team_ids
    }

    team_set = set(team_ids)

    for match in rows_matches:
        if match["home"] not in team_set or match["away"] not in team_set:
            continue

        # Future/incomplete tournament editions may legitimately contain
        # scheduled fixtures with no score yet. They belong to the tournament
        # structure, but must not count in the live standings.
        if match.get("played") is False:
            continue

        if match.get("homeScore") in (None, "") or match.get("awayScore") in (None, ""):
            continue

        home = rows[match["home"]]
        away = rows[match["away"]]

        home_score = _match_score(match, "home")
        away_score = _match_score(match, "away")

        home["p"] += 1
        away["p"] += 1

        home["scoreFor"] += home_score
        home["scoreAgainst"] += away_score
        away["scoreFor"] += away_score
        away["scoreAgainst"] += home_score

        if home_score > away_score:
            home["w"] += 1
            away["l"] += 1
            home["pts"] += points["win"]
            away["pts"] += points["loss"]

        elif away_score > home_score:
            away["w"] += 1
            home["l"] += 1
            away["pts"] += points["win"]
            home["pts"] += points["loss"]

        else:
            home["d"] += 1
            away["d"] += 1
            home["pts"] += points["draw"]
            away["pts"] += points["draw"]

        home_bonus, away_bonus = _bonus_points_for_match(
            match,
            home_score,
            away_score,
            stage,
        )

        home["bonus"] += home_bonus
        away["bonus"] += away_bonus
        home["pts"] += home_bonus
        away["pts"] += away_bonus

    for row in rows.values():
        row["scoreDifference"] = row["scoreFor"] - row["scoreAgainst"]

        # Legacy aliases used by the existing football ordering logic.
        row["gf"] = row["scoreFor"]
        row["ga"] = row["scoreAgainst"]
        row["gd"] = row["scoreDifference"]

    return rows


def exact_group_order(
    stage: dict[str, Any],
    raw_matches: list[dict[str, Any]],
    groups: dict[str, list[str]],
    qualifier_ids: set[str] | list[str],
    edition_id: str,
):
    standings_count = int(stage.get("standingsMatchCount", len(raw_matches)))
    standings_matches = raw_matches[:standings_count]
    points = actual_points(stage, edition_id)
    qualifier_ids = set(qualifier_ids or [])
    result: dict[str, list[str]] = {}

    for label, team_ids in groups.items():
        rows = standings(
            standings_matches,
            team_ids,
            points,
            stage,
        )

        # Default ordering is deliberately conservative and matches the existing
        # International Football generator:
        #   competition points -> score difference -> score for
        #
        # Qualifier priority is used only after those values are tied. Historical
        # or sport-specific tie-break outcomes that cannot be reconstructed from
        # the available result JSON belong in edition overrides.
        result[label] = sorted(
            team_ids,
            key=lambda team: (
                -rows[team]["pts"],
                -rows[team]["scoreDifference"],
                -rows[team]["scoreFor"],
                0 if team in qualifier_ids else 1,
                team,
            ),
        )

    return result


# ---------------------------------------------------------------------------
# Knockout construction
# ---------------------------------------------------------------------------

def dedupe_last(matches: list[dict[str, Any]]) -> list[dict[str, Any]]:
    last: dict[frozenset[str], dict[str, Any]] = {}
    order: list[frozenset[str]] = []

    for match in matches:
        key = frozenset((match["home"], match["away"]))

        if key not in last:
            order.append(key)

        last[key] = match

    return [last[key] for key in order]


def build_knockout_rounds(
    data: dict[str, Any],
    template: dict[str, Any],
    start_index: int,
):
    raw_matches = data["matches"]
    rounds: list[dict[str, Any]] = []
    cursor = start_index

    for round_def in template.get("knockoutRounds", []):
        count = int(round_def["rawMatchCount"])
        raw = raw_matches[cursor:cursor + count]
        cursor += count

        selected = (
            dedupe_last(raw)
            if round_def.get("dedupeReplay") == "last"
            else raw
        )

        matches: list[dict[str, Any]] = []

        for idx, match in enumerate(selected, start=1):
            matches.append({
                "id": f"{round_def['id']}-{idx}",
                "teams": [match["home"], match["away"]],
                "date": match["date"],
            })

        rounds.append({
            "id": round_def["id"],
            "label": round_def["label"],
            "matches": matches,
        })

    # Add byes where a team entered the next round without appearing in the
    # preceding round.
    for idx, round_def in enumerate(template.get("knockoutRounds", [])):
        if not round_def.get("addByeFromNextRound"):
            continue

        if idx + 1 >= len(rounds):
            continue

        current_teams = {
            team
            for match in rounds[idx]["matches"]
            for team in match["teams"]
            if team
        }

        next_teams = {
            team
            for match in rounds[idx + 1]["matches"]
            for team in match["teams"]
            if team
        }

        bye_teams = sorted(next_teams - current_teams)

        for team in bye_teams:
            rounds[idx]["matches"].append({
                "id": (
                    f"{rounds[idx]['id']}-bye-"
                    f"{len(rounds[idx]['matches']) + 1}"
                ),
                "teams": [team, None],
                "label": "Bye",
            })

    # Feed ordinary main-round matches into the next ordinary main round.
    # Placement rounds such as third-place are excluded from the main chain.
    excluded_from_main_chain = set(
        template.get(
            "excludeRoundsFromMainChain",
            ["third-place"],
        )
    )

    main = [
        round_obj
        for round_obj in rounds
        if round_obj["id"] not in excluded_from_main_chain
    ]

    for idx in range(len(main) - 1):
        current = main[idx]
        next_round = main[idx + 1]

        for source_match in current["matches"]:
            source_teams = {
                team
                for team in source_match["teams"]
                if team
            }

            candidates = [
                next_match
                for next_match in next_round["matches"]
                if source_teams.intersection(
                    team
                    for team in next_match["teams"]
                    if team
                )
            ]

            if len(candidates) == 1:
                source_match["feedsInto"] = candidates[0]["id"]

    return rounds, cursor


# ---------------------------------------------------------------------------
# Structure generation
# ---------------------------------------------------------------------------

def build_structure(
    data: dict[str, Any],
    template: dict[str, Any],
    override: dict[str, Any] | None = None,
):
    matches = data["matches"]
    cursor = 0
    stage_work: list[dict[str, Any]] = []

    group_stage_defs = template.get("groupStages", [])

    for idx, stage_def in enumerate(group_stage_defs):
        count = int(stage_def["matchCount"])
        raw = matches[cursor:cursor + count]

        if len(raw) != count:
            allow_incomplete = bool(stage_def.get("allowIncomplete", False))

            if not (allow_incomplete and len(raw) < count):
                raise RuntimeError(
                    f"{data['editionId']}: {stage_def['label']} expected "
                    f"{count} matches, found {len(raw)}"
                )

        stage_id = (
            "group-stage"
            if len(group_stage_defs) == 1
            else f"group-stage-{idx + 1}"
        )

        label_anchors = (
            (override or {})
            .get("groupAnchors", {})
            .get(stage_id)
        )

        groups = stage_groups(
            stage_def,
            raw,
            label_anchors,
        )

        stage_work.append({
            "id": stage_id,
            "definition": stage_def,
            "rawMatches": raw,
            "groups": groups,
        })

        cursor += len(raw)

    knockout_rounds, cursor = build_knockout_rounds(
        data,
        template,
        cursor,
    )

    if cursor != len(matches):
        raise RuntimeError(
            f"{data['editionId']}: template consumed {cursor} of "
            f"{len(matches)} matches"
        )

    knockout_qualifier_ids = {
        team
        for round_obj in knockout_rounds
        for match in round_obj.get("matches", [])
        for team in match.get("teams", [])
        if team
    }

    output_stages: list[dict[str, Any]] = []

    for idx, work in enumerate(stage_work):
        stage_def = work["definition"]
        raw = work["rawMatches"]
        groups = work["groups"]

        if idx + 1 < len(stage_work):
            qualifier_ids = {
                team
                for group in stage_work[idx + 1]["groups"].values()
                for team in group
            }

        elif knockout_qualifier_ids:
            qualifier_ids = knockout_qualifier_ids

        else:
            qualifier_ids = set()

        final_order = exact_group_order(
            stage_def,
            raw,
            groups,
            qualifier_ids,
            str(data["editionId"]),
        )

        stage_obj = {
            "id": work["id"],
            "label": stage_def["label"],
            "startDate": raw[0]["date"],
            "endDate": raw[-1]["date"],
            "points": actual_points(stage_def, str(data["editionId"])),
            "tiebreakers": stage_def.get(
                "tiebreakers",
                ["points", "goalDifference", "goalsFor"],
            ),
            "groups": groups,
            "finalOrder": final_order,
            "qualifierIds": sorted(qualifier_ids),
        }

        if stage_def.get("bonusPoints"):
            stage_obj["bonusPoints"] = stage_def["bonusPoints"]

        standings_count = stage_def.get("standingsMatchCount")

        if standings_count and int(standings_count) < len(raw):
            stage_obj["standingsEndDate"] = (
                raw[int(standings_count) - 1]["date"]
            )

        output_stages.append(stage_obj)

    return {
        "tournamentId": data["tournamentId"],
        "editionId": data["editionId"],
        "renderer": template.get("renderer", "group_knockout"),
        "formatTemplate": template["id"],
        "groupStages": output_stages,
        "knockoutRounds": knockout_rounds,
    }


# ---------------------------------------------------------------------------
# Edition overrides
# ---------------------------------------------------------------------------

def apply_override(
    structure: dict[str, Any],
    override: dict[str, Any] | None,
):
    if not override:
        return structure

    round_map = {
        round_obj["id"]: round_obj
        for round_obj in structure.get("knockoutRounds", [])
    }

    for item in override.get("addKnockoutMatches", []):
        round_id = item["roundId"]

        if round_id not in round_map:
            raise RuntimeError(
                f"Override refers to unknown round {round_id!r}"
            )

        round_map[round_id]["matches"].append(item["match"])

    stage_map = {
        stage["id"]: stage
        for stage in structure.get("groupStages", [])
    }

    for stage_id, group_orders in override.get(
        "finalOrderOverrides",
        {},
    ).items():
        if stage_id not in stage_map:
            raise RuntimeError(
                f"Override refers to unknown stage {stage_id!r}"
            )

        stage = stage_map[stage_id]

        for group_name, order in group_orders.items():
            teams = stage.get("groups", {}).get(group_name)

            if teams is None:
                raise RuntimeError(
                    f"Override refers to unknown group "
                    f"{stage_id}/{group_name}"
                )

            if set(order) != set(teams):
                raise RuntimeError(
                    f"Override order does not match teams in "
                    f"{stage_id}/{group_name}"
                )

            stage["finalOrder"][group_name] = order

    # Optional generic patch point for display-only structure metadata.
    # This deliberately does not touch the source tournament-results JSON.
    for path_key, value in override.get("set", {}).items():
        parts = path_key.split(".")
        target: Any = structure

        for part in parts[:-1]:
            if not isinstance(target, dict) or part not in target:
                raise RuntimeError(
                    f"Override set path does not exist: {path_key}"
                )
            target = target[part]

        if not isinstance(target, dict):
            raise RuntimeError(
                f"Override set path is not a dictionary target: {path_key}"
            )

        target[parts[-1]] = value

    return structure


# ---------------------------------------------------------------------------
# Validation
# ---------------------------------------------------------------------------

def validate_structure(
    data: dict[str, Any],
    structure: dict[str, Any],
) -> list[str]:
    team_ids = {
        team["id"]
        for team in data.get("teams", [])
    }

    errors: list[str] = []

    for stage in structure.get("groupStages", []):
        for label, teams in stage.get("groups", {}).items():
            missing = set(teams) - team_ids

            if missing:
                errors.append(
                    f"{stage['label']} Group {label}: "
                    f"unknown teams {sorted(missing)}"
                )

            final_order = (
                stage.get("finalOrder", {})
                .get(label, [])
            )

            if set(final_order) != set(teams):
                errors.append(
                    f"{stage['label']} Group {label}: "
                    f"finalOrder mismatch"
                )

    data_fixtures = {
        (
            match["date"],
            frozenset((match["home"], match["away"])),
        )
        for match in data.get("matches", [])
    }

    for round_obj in structure.get("knockoutRounds", []):
        for structure_match in round_obj.get("matches", []):
            if structure_match.get("sourceDataMissing"):
                continue

            teams = [
                team
                for team in structure_match.get("teams", [])
                if team
            ]

            if len(teams) < 2:
                continue

            key = (
                structure_match.get("date"),
                frozenset(teams),
            )

            if key not in data_fixtures:
                errors.append(
                    f"{round_obj['label']}: fixture not found "
                    f"in tournament data: {structure_match}"
                )

    return errors


# ---------------------------------------------------------------------------
# Sport / tournament discovery
# ---------------------------------------------------------------------------

def sport_data_root(root: Path, sport: str) -> Path:
    return root / sport / "data"


def sport_has_tournament_system(root: Path, sport: str) -> bool:
    format_root = (
        sport_data_root(root, sport)
        / "tournament-formats"
    )

    return (
        format_root.is_dir()
        and any(
            child.is_dir() and any(child.glob("*.json"))
            for child in format_root.iterdir()
        )
    )


def available_sports(root: Path) -> list[str]:
    """
    Discover sports from repository folders containing:
        <Sport>/data/tournament-formats/<tournament>/*.json
    """
    sports: list[str] = []

    for child in sorted(root.iterdir()):
        if not child.is_dir():
            continue

        if sport_has_tournament_system(root, child.name):
            sports.append(child.name)

    return sports


def available_tournaments(
    root: Path,
    sport: str,
) -> list[str]:
    format_root = (
        sport_data_root(root, sport)
        / "tournament-formats"
    )

    if not format_root.exists():
        return []

    return sorted(
        path.name
        for path in format_root.iterdir()
        if path.is_dir() and any(path.glob("*.json"))
    )


# ---------------------------------------------------------------------------
# Processing
# ---------------------------------------------------------------------------

def process_tournament(
    root: Path,
    sport: str,
    tournament_id: str,
    edition: str | None = None,
    check_only: bool = False,
) -> int:
    data_root = sport_data_root(root, sport)

    data_dir = (
        data_root
        / "tournaments"
        / tournament_id
    )

    format_dir = (
        data_root
        / "tournament-formats"
        / tournament_id
    )

    output_dir = (
        data_root
        / "tournament-structure"
        / tournament_id
    )

    override_dir = (
        data_root
        / "tournament-overrides"
        / tournament_id
    )

    if not data_dir.exists():
        raise RuntimeError(
            f"{sport}/{tournament_id}: "
            f"tournament data folder not found."
        )

    if not format_dir.exists():
        raise RuntimeError(
            f"{sport}/{tournament_id}: "
            f"format-template folder not found."
        )

    templates = [
        load_json(path)
        for path in sorted(format_dir.glob("*.json"))
    ]

    if not templates:
        raise RuntimeError(
            f"{sport}/{tournament_id}: "
            f"no format templates found."
        )

    edition_paths = sorted(
        path
        for path in data_dir.glob("*.json")
        if path.stem not in {"editions", "index"}
    )

    if edition:
        edition_paths = [
            path
            for path in edition_paths
            if path.stem == edition
        ]

        if not edition_paths:
            raise RuntimeError(
                f"{sport}/{tournament_id}: "
                f"edition {edition} not found."
            )

    failures = 0

    for path in edition_paths:
        data = load_json(path)

        try:
            template = choose_template(
                templates,
                data,
            )

            override_path = (
                override_dir
                / f"{data['editionId']}.json"
            )

            override = (
                load_json(override_path)
                if override_path.exists()
                else None
            )

            structure = build_structure(
                data,
                template,
                override,
            )

            structure = apply_override(
                structure,
                override,
            )

            errors = validate_structure(
                data,
                structure,
            )

            if errors:
                failures += 1
                print(
                    f"{sport} {tournament_id} "
                    f"{data['editionId']}: FAILED"
                )

                for error in errors:
                    print(f"  - {error}")

                continue

            if not check_only:
                save_json(
                    output_dir / f"{data['editionId']}.json",
                    structure,
                )

            print(
                f"{sport} {tournament_id} "
                f"{data['editionId']}: {template['id']} "
                f"-> {'checked' if check_only else 'written'}"
            )

        except Exception as exc:
            failures += 1
            print(
                f"{sport} {tournament_id} "
                f"{path.stem}: FAILED - {exc}"
            )

    return failures


def process_sport(
    root: Path,
    sport: str,
    tournament: str | None = None,
    edition: str | None = None,
    check_only: bool = False,
) -> int:
    tournaments = (
        [tournament]
        if tournament
        else available_tournaments(root, sport)
    )

    if not tournaments:
        raise RuntimeError(
            f"{sport}: no tournament format-template folders found."
        )

    failures = 0

    for tournament_id in tournaments:
        try:
            failures += process_tournament(
                root=root,
                sport=sport,
                tournament_id=tournament_id,
                edition=edition,
                check_only=check_only,
            )

        except Exception as exc:
            failures += 1
            print(
                f"{sport}/{tournament_id}: FAILED - {exc}"
            )

    return failures


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

def main() -> None:
    parser = argparse.ArgumentParser(
        description=(
            "Generate and validate tournament structure JSON across "
            "J-Ratings sports."
        )
    )

    parser.add_argument(
        "--sport",
        help=(
            "Repository sport folder, e.g. InternationalFootball or RugbyUnion. "
            "Omit to process every sport that has tournament format templates."
        ),
    )

    parser.add_argument(
        "--tournament",
        help=(
            "Generate one tournament family, e.g. world-cup. "
            "When more than one sport is available, use with --sport."
        ),
    )

    parser.add_argument(
        "--edition",
        help=(
            "Generate one edition, e.g. 2018. "
            "Requires --sport and --tournament."
        ),
    )

    parser.add_argument(
        "--check-only",
        action="store_true",
        help="Validate generation without writing structure files.",
    )

    parser.add_argument(
        "--list",
        action="store_true",
        help="List detected sports and tournament families, then exit.",
    )

    args = parser.parse_args()

    root = repo_root()

    if args.edition and (not args.sport or not args.tournament):
        raise SystemExit(
            "--edition requires both --sport and --tournament."
        )

    if args.tournament and not args.sport:
        sports = available_sports(root)

        if len(sports) != 1:
            raise SystemExit(
                "--tournament without --sport is only allowed when exactly "
                "one tournament-enabled sport is detected."
            )

        args.sport = sports[0]

    sports = (
        [args.sport]
        if args.sport
        else available_sports(root)
    )

    if args.list:
        if not sports:
            print("No tournament-enabled sports detected.")
            return

        for sport in sports:
            print(sport)

            for tournament_id in available_tournaments(root, sport):
                print(f"  {tournament_id}")

        return

    if not sports:
        raise SystemExit(
            "No sport folders containing data/tournament-formats were found."
        )

    failures = 0

    for sport in sports:
        sport_dir = root / sport

        if not sport_dir.is_dir():
            failures += 1
            print(f"{sport}: FAILED - sport folder not found.")
            continue

        failures += process_sport(
            root=root,
            sport=sport,
            tournament=args.tournament,
            edition=args.edition,
            check_only=args.check_only,
        )

    if failures:
        raise SystemExit(
            f"{failures} edition/tournament failure(s)."
        )

    print(
        "All requested tournament structures generated successfully."
    )


if __name__ == "__main__":
    main()
