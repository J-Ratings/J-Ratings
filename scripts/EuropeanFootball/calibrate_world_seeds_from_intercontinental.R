# Calibrate non-UEFA starting seeds from cached intercontinental club matches.
#
# Sources: FIFA Club World Cup (2000, 2005-2025) and the Intercontinental Cup
# (1980-2004).  No web requests and no production files are changed.
#
# This measures *elite entrants*, mostly continental champions.  Therefore the
# implied Elo gaps are deliberately reported as champion-level gaps; they are
# evidence for setting league seeds, not automatic league-seed replacements.

run_world_seed_calibration <- function() {
  needed <- c("data.table", "xml2", "stringi", "tictoc", "beepr")
  absent <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
  if (length(absent)) stop("Install these packages first: ", paste(absent, collapse = ", "))
  tictoc::tic("Intercontinental seed calibration")
  on.exit({ tictoc::toc(); try(suppressWarnings(beepr::beep()), silent = TRUE) }, add = TRUE)

  root <- normalizePath(Sys.getenv("J_RATINGS_REPO", "C:/Users/stjuk/Documents/GitHub/J-Ratings"),
                        winslash = "/", mustWork = TRUE)
  pages <- file.path(root, "EuropeanFootball/pipeline_data/Source/rsssf/all/pages")
  out <- file.path(root, "EuropeanFootball/pipeline_data/Manual_Sources/RSSSF_World_Audit/intercontinental_seed_calibration")
  dir.create(out, recursive = TRUE, showWarnings = FALSE)

  clean <- function(x) trimws(gsub("[[:space:]]+", " ", stringi::stri_trans_nfkc(x)))
  norm <- function(x) gsub("[^a-z0-9]", "", tolower(stringi::stri_trans_general(clean(x), "Latin-ASCII")))
  confeds <- c("UEFA", "CONMEBOL", "CONCACAF", "CAF", "AFC", "OFC")

  # Countries used in Club World Cup participant lists.  Additions here are
  # transparent and safer than guessing a club's country from its name.
  country_confed <- c(
    "Algeria"="CAF", "Angola"="CAF", "Argentina"="CONMEBOL", "Australia"="AFC", "Brazil"="CONMEBOL",
    "Cameroon"="CAF", "Canada"="CONCACAF", "China"="AFC", "Colombia"="CONMEBOL", "Congo-Kinshasa"="CAF",
    "Costa Rica"="CONCACAF", "DR Congo"="CAF", "Ecuador"="CONMEBOL", "Egypt"="CAF", "England"="UEFA",
    "France"="UEFA", "Germany"="UEFA", "Japan"="AFC", "Mexico"="CONCACAF", "Morocco"="CAF",
    "New Zealand"="OFC", "Nigeria"="CAF", "Paraguay"="CONMEBOL", "Portugal"="UEFA", "Qatar"="AFC",
    "Saudi Arabia"="AFC", "South Africa"="CAF", "South Korea"="AFC", "Spain"="UEFA", "Tahiti"="OFC",
    "Tunisia"="CAF", "UAE"="AFC", "United Arab Emirates"="AFC", "United States"="CONCACAF",
    "Uruguay"="CONMEBOL", "Venezuela"="CONMEBOL", "Italy"="UEFA", "Netherlands"="UEFA", "Greece"="UEFA",
    "Turkey"="UEFA", "Romania"="UEFA", "Russia"="UEFA", "Serbia"="UEFA", "Croatia"="UEFA",
    "Ukraine"="UEFA", "Switzerland"="UEFA", "Scotland"="UEFA", "Belgium"="UEFA", "Austria"="UEFA"
  )
  code_confed <- c(ITA="UEFA", ESP="UEFA", ENG="UEFA", GER="UEFA", POR="UEFA", NED="UEFA", FRA="UEFA",
                   SCO="UEFA", ROU="UEFA", SRB="UEFA", CRO="UEFA", UKR="UEFA", GRE="UEFA",
                   BRA="CONMEBOL", ARG="CONMEBOL", URU="CONMEBOL", PAR="CONMEBOL", COL="CONMEBOL", CHI="CONMEBOL",
                   MEX="CONCACAF", USA="CONCACAF", CRC="CONCACAF", HON="CONCACAF",
                   EGY="CAF", MAR="CAF", RSA="CAF", TUN="CAF", CMR="CAF", COD="CAF", NGA="CAF",
                   JPN="AFC", KOR="AFC", KSA="AFC", UAE="AFC", QAT="AFC", CHN="AFC", AUS="AFC", IRN="AFC",
                   NZL="OFC")

  nearest_participant <- function(team, participants) {
    if (!nrow(participants)) return(NA_character_)
    z <- norm(team); p <- participants$Key
    exact <- which(z == p)
    if (length(exact)) return(participants$Confederation[exact[1L]])
    # Restrict fuzzy acceptance to a distinctive containment relationship.
    hit <- which(nchar(z) >= 5L & (startsWith(p, z) | startsWith(z, p)))
    if (length(hit) == 1L) return(participants$Confederation[hit])
    # Fixture rows regularly omit FC/SC/Club and the city in brackets. Match
    # the distinctive club words, but only if exactly one participant fits.
    tokens <- function(x) {
      y <- unlist(strsplit(tolower(stringi::stri_trans_general(x, "Latin-ASCII")), "[^a-z0-9]+"))
      setdiff(y[nchar(y) >= 3L], c("football", "club", "city", "united", "real", "athletic", "atletico", "fc", "sc", "ac", "cf"))
    }
    tx <- tokens(team)
    score <- vapply(participants$Team, function(candidate) {
      tc <- tokens(candidate)
      if (!length(tx) || !length(tc)) return(0)
      length(intersect(tx, tc)) / min(length(tx), length(tc))
    }, numeric(1))
    hit <- which(score >= .8)
    if (length(hit) == 1L) return(participants$Confederation[hit])
    NA_character_
  }
  html_lines <- function(path) {
    doc <- xml2::read_html(path)
    x <- xml2::xml_text(xml2::xml_find_all(doc, "//pre"))
    clean(unlist(strsplit(paste(x, collapse = "\n"), "\n", fixed = TRUE)))
  }

  parse_fifa <- function(path) {
    lines <- html_lines(path)
    title <- clean(xml2::xml_text(xml2::read_html(path) |> xml2::xml_find_first("//title")))
    year <- as.integer(sub(".*?(20[0-9]{2}).*", "\\1", title))
    if (is.na(year)) year <- as.integer(sub(".*?(00|0[5-9]).*", "20\\1", basename(path)))
    # Participant rows precede the first round heading. They give the club's
    # country, which is preferable to inferring it from a shortened fixture name.
    stop_at <- which(grepl("^(Round|Group|Semi|Quarter|Final|Play[- ]?off|First Match)", lines, ignore.case = TRUE))[1L]
    p_lines <- lines[seq_len(ifelse(is.na(stop_at), length(lines), max(1L, stop_at - 1L)))]
    participants <- rbindlist(lapply(p_lines, function(x) {
      # Older editions describe qualification after the country; the expanded
      # 2025 edition simply lists confederation, club and country.  Both have
      # a country in parentheses, and this pre-fixture section has no result
      # rows to confuse with a participant.
      original <- x
      x <- sub("^(?:AFC|CAF|CONCACAF|CONMEBOL|OFC|UEFA)[[:space:]]+", "", x)
      m <- regexec("^(.+?) \\(([^()]+)\\)", x, perl = TRUE)
      q <- regmatches(x, m)[[1L]]
      if (length(q) != 3L) return(NULL)
      inferred <- if (grepl("UEFA", original, ignore.case = TRUE)) "UEFA"
      else if (grepl("Libertadores|CONMEBOL", original, ignore.case = TRUE)) "CONMEBOL"
      else if (grepl("Asian", original, ignore.case = TRUE)) "AFC"
      else if (grepl("African", original, ignore.case = TRUE)) "CAF"
      else if (grepl("Oceanian|OFC", original, ignore.case = TRUE)) "OFC"
      else if (grepl("CONCACAF|FC Champions Cup", original, ignore.case = TRUE)) "CONCACAF"
      else unname(country_confed[q[3L]])
      if (is.na(inferred)) return(NULL)
      data.table(Team = clean(q[2L]), Key = norm(q[2L]), Confederation = inferred)
    }), fill = TRUE)
    # RSSSF FIFA layouts put a date, optional match number/venue, then teams.
    # The score is the only required anchor; unclassified rows stay in review.
    participant_in <- function(x, candidates, side) {
      nx <- norm(x)
      hit <- candidates[nchar(Key) >= 4L & vapply(Key, function(k) grepl(k, nx, fixed = TRUE), logical(1))]
      if (!nrow(hit)) return(NA_character_)
      # For the left side of a score use the last named club; for the right
      # side use the first.  The longest match resolves abbreviated variants.
      hit <- hit[order(-nchar(Key))]
      hit$Team[1L]
    }
    fixtures <- rbindlist(lapply(lines, function(x) {
      m <- regexec("^([0-9]{1,2}-[[:space:]]*[0-9]{1,2}-[0-9]{2,4}.*?)\\b([0-9]+)-([0-9]+)\\b(.*)$", x, perl = TRUE)
      q <- regmatches(x, m)[[1L]]
      if (length(q) != 5L) return(NULL)
      home <- participant_in(q[2L], participants, "left")
      away <- participant_in(q[5L], participants, "right")
      if (is.na(home) || is.na(away)) return(NULL)
      data.table(Source = "FIFA Club World Cup", Season = as.character(year), Home = home,
                 Away = away, HG = as.integer(q[3L]), AG = as.integer(q[4L]), RawLine = x)
    }), fill = TRUE)
    if (!nrow(fixtures)) return(fixtures)
    fixtures[, `:=`(HomeConfederation = vapply(Home, nearest_participant, character(1), participants = participants),
                    AwayConfederation = vapply(Away, nearest_participant, character(1), participants = participants))]
    fixtures
  }

  parse_toyota <- function(path) {
    lines <- html_lines(path)
    yr <- as.integer(sub(".*?([0-9]{2})\\.html", "\\1", basename(path)))
    year <- ifelse(yr <= 25L, 2000L + yr, 1900L + yr)
    ans <- rbindlist(lapply(lines, function(x) {
      m <- regexec("^(.+?)\\s+\\(([A-Z]{3})\\)\\s+([0-9]+)-([0-9]+)(?:\\s+\\([0-9-]+\\))?\\s+(.+?)\\s+\\(([A-Z]{3})\\)", x, perl = TRUE)
      q <- regmatches(x, m)[[1L]]
      if (length(q) != 7L) return(NULL)
      data.table(Source = "Intercontinental Cup", Season = as.character(year), Home = clean(q[2L]), Away = clean(q[5L]),
                 HG = as.integer(q[4L]), AG = as.integer(q[5L]), HomeConfederation = unname(code_confed[q[3L]]),
                 AwayConfederation = unname(code_confed[q[7L]]), RawLine = x)
    }), fill = TRUE)
    ans
  }

  fifa_files <- list.files(file.path(pages, "tablesf"), "^fifa-wcc(00|0[5-9]|20[0-9]{2})\\.html$", full.names = TRUE)
  toyota_files <- list.files(file.path(pages, "tablest"), "^toyota(0[0-4]|[89][0-9]|9[0-9])\\.html$", full.names = TRUE)
  games <- rbindlist(c(lapply(fifa_files, parse_fifa), lapply(toyota_files, parse_toyota)), fill = TRUE)
  # The user supplied a Wikipedia fixture inventory for the Club World Cup.
  # It is deliberately stored beside this analysis, so the evidence remains
  # reproducible and we do not scrape Wikipedia again.  Prefer it over the
  # RSSSF Club World Cup layouts, while retaining RSSSF Intercontinental Cups.
  wiki_file <- file.path(out, "wikipedia_club_world_cup_results.tsv")
  if (file.exists(wiki_file)) {
    club_groups <- list(
      UEFA = c("AC Milan", "Atlético Madrid", "Barcelona", "Bayern Munich", "Benfica", "Borussia Dortmund", "Chelsea", "Inter Milan", "Juventus", "Liverpool", "Manchester City", "Manchester United", "Paris Saint-Germain", "Porto", "Real Madrid", "Red Bull Salzburg"),
      CONMEBOL = c("Atlético Mineiro", "Atlético Nacional", "Boca Juniors", "Botafogo", "Corinthians", "Estudiantes", "Flamengo", "Fluminense", "Grêmio", "Internacional", "LDU Quito", "Palmeiras", "River Plate", "San Lorenzo", "Santos", "São Paulo", "Vasco da Gama"),
      CONCACAF = c("América", "Atlante", "Cruz Azul", "Guadalajara", "Inter Miami", "León", "Los Angeles FC", "Monterrey", "Necaxa", "Pachuca", "Saprissa", "Seattle Sounders", "Tigres UANL"),
      CAF = c("Étoile du Sahel", "Al Ahly", "ES Sétif", "Espérance de Tunis", "Mamelodi Sundowns", "Moghreb Tétouan", "Raja Casablanca", "TP Mazembe", "Wydad Casablanca"),
      AFC = c("Adelaide United", "Al-Ahli Dubai", "Al-Ain", "Al-Duhail", "Al-Hilal", "Al-Ittihad", "Al-Jazira", "Al-Nassr", "Al-Sadd", "Al-Wahda", "Guangzhou Evergrande", "Gamba Osaka", "Jeonbuk Hyundai Motors", "Kashima Antlers", "Kashiwa Reysol", "Pohang Steelers", "Sanfrecce Hiroshima", "Seongnam Ilhwa Chunma", "Sepahan", "Sydney FC", "Ulsan HD", "Ulsan Hyundai", "Urawa Red Diamonds", "Western Sydney Wanderers"),
      OFC = c("AS Pirae", "Auckland City", "Hekari United", "Hienghène Sport", "South Melbourne", "Team Wellington", "Waitakere United")
    )
    club_confed <- unlist(lapply(names(club_groups), function(conf) setNames(rep(conf, length(club_groups[[conf]])), norm(club_groups[[conf]]))))
    wiki <- data.table::fread(wiki_file, encoding = "UTF-8")
    setnames(wiki, c("Team 1", "Team 2"), c("Home", "Away"))
    wiki[, `:=`(HomeConfederation = unname(club_confed[norm(Home)]),
                AwayConfederation = unname(club_confed[norm(Away)]))]
    wiki[, c("HG", "AG") := {
      sc <- regexec("^[[:space:]]*([0-9]+)[^0-9]+([0-9]+)", Score, perl = TRUE)
      x <- regmatches(Score, sc)
      list(as.integer(vapply(x, function(z) if (length(z) > 2L) z[2L] else NA_character_, character(1))),
           as.integer(vapply(x, function(z) if (length(z) > 2L) z[3L] else NA_character_, character(1))))
    }]
    wiki_games <- wiki[!is.na(HG) & !is.na(AG), .(Source = "Wikipedia Club World Cup", Season = as.character(Year),
      Home, Away, HG, AG, HomeConfederation, AwayConfederation, RawLine = paste(Year, Stage, Home, Score, Away))]
    unresolved <- unique(c(wiki_games[is.na(HomeConfederation), Home], wiki_games[is.na(AwayConfederation), Away]))
    fwrite(data.table(Team = unresolved), file.path(out, "wikipedia_club_world_cup_unmapped_teams.csv"))
    games <- rbindlist(list(games[Source == "Intercontinental Cup"], wiki_games), fill = TRUE)
  }
  if (!nrow(games)) stop("No cached intercontinental results were parsed.")
  games <- games[HomeConfederation %in% confeds & AwayConfederation %in% confeds & HomeConfederation != AwayConfederation]
  games[, HomePoints := fifelse(HG > AG, 1, fifelse(HG == AG, .5, 0))]
  games[, AwayPoints := 1 - HomePoints]
  write.csv(games, file.path(out, "parsed_intercontinental_matches.csv"), row.names = FALSE, na = "")

  long <- rbindlist(list(
    games[, .(Source, Season, Confederation = HomeConfederation, Opponent = AwayConfederation, Points = HomePoints)],
    games[, .(Source, Season, Confederation = AwayConfederation, Opponent = HomeConfederation, Points = AwayPoints)]
  ))
  versus_uefa <- long[Opponent == "UEFA", .(Games = .N, Points = sum(Points),
    ObservedPointPct = round(100 * mean(Points), 1)), by = Confederation]
  # Add two neutral pseudo-games: avoids treating a handful of matches as a
  # definitive 0 or 100 percent result.  Elo gap is from expected score p.
  versus_uefa[, SmoothedPointPct := (Points + 1) / (Games + 2)]
  versus_uefa[, ChampionEloGapVsUEFA := round(400 * log10(SmoothedPointPct / (1 - SmoothedPointPct)))]
  versus_uefa <- versus_uefa[order(-Games)]
  write.csv(versus_uefa, file.path(out, "confederation_vs_uefa.csv"), row.names = FALSE)

  quality <- games[, .(ParsedCrossConfederationGames = .N,
                         UnclassifiedOrSameConfederationRowsExcluded = 0L)]
  write.csv(quality, file.path(out, "run_summary.csv"), row.names = FALSE)
  print(versus_uefa)
  message("Wrote calibration evidence to: ", out)
  message("No seeds, matches, aliases, or site files were changed.")
}

run_world_seed_calibration()
