# Import the cached RSSSF Toyota Cup (1980-2004) and FIFA Intercontinental
# Cup (2024 onward) results into the production match master.
#
# Run from RStudio:
# source("C:/Users/stjuk/Documents/GitHub/J-Ratings/scripts/EuropeanFootball/import_fifa_intercontinental_cup.R")
#
# Optional audit-only run:
# Sys.setenv(ICC_IMPORT_DRY_RUN = "1")

suppressPackageStartupMessages({
  library(data.table)
  library(tictoc)
  library(beepr)
})

tictoc::tic("FIFA Intercontinental Cup import")
on.exit({tictoc::toc(); try(suppressWarnings(beepr::beep()), silent = TRUE)}, add = TRUE)

root <- normalizePath(Sys.getenv(
  "J_RATINGS_REPO", "C:/Users/stjuk/Documents/GitHub/J-Ratings"
), winslash = "/", mustWork = TRUE)

master_path <- file.path(
  root, "EuropeanFootball/pipeline_data/Matches_Clean_Combined/european_football_all_matches.csv"
)
ratings_path <- file.path(
  root, "EuropeanFootball/pipeline_data/Elo/football_elo_final_ratings.csv"
)
rsssf_root <- file.path(root, "EuropeanFootball/pipeline_data/Source/rsssf/all/pages")
audit_dir <- file.path(
  root, "EuropeanFootball/pipeline_data/Manual_Sources/RSSSF_World_Audit/fifa_intercontinental_cup_import"
)
dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(master_path) || !file.exists(ratings_path)) {
  stop("Required master or current Elo ratings file is missing.")
}

# Canonical names deliberately match 02_calculate_elo.R identities. Explicit
# associations prevent ambiguous names such as Nacional, Liverpool and Ahly
# from being assigned to a club in the wrong country.
toyota <- data.table::rbindlist(list(
  list("1980","1981-02-11","Nacional","Uruguay","Nottingham Forest","England",1L,0L,"toyota80.html"),
  list("1981","1981-12-13","Flamengo","Brazil","Liverpool","England",3L,0L,"toyota81.html"),
  list("1982","1982-12-12","Peñarol","Uruguay","Aston Villa","England",2L,0L,"toyota82.html"),
  list("1983","1983-12-11","Grêmio","Brazil","Hamburg","Germany",2L,1L,"toyota83.html"),
  list("1984","1984-12-09","Independiente","Argentina","Liverpool","England",1L,0L,"toyota84.html"),
  list("1985","1985-12-08","Juventus","Italy","Argentinos Juniors","Argentina",2L,2L,"toyota85.html"),
  list("1986","1986-12-14","River Plate","Argentina","Steaua București","Romania",1L,0L,"toyota86.html"),
  list("1987","1987-12-13","Porto","Portugal","Peñarol","Uruguay",2L,1L,"toyota87.html"),
  list("1988","1988-12-11","Nacional","Uruguay","PSV","Netherlands",2L,2L,"toyota88.html"),
  list("1989","1989-12-17","AC Milan","Italy","Nacional","Colombia",1L,0L,"toyota89.html"),
  list("1990","1990-12-09","AC Milan","Italy","Olimpia","Paraguay",3L,0L,"toyota90.html"),
  list("1991","1991-12-08","Red Star Belgrade","Serbia","Colo-Colo","Chile",3L,0L,"toyota91.html"),
  list("1992","1992-12-13","São Paulo","Brazil","Barcelona","Spain",2L,1L,"toyota92.html"),
  list("1993","1993-12-12","São Paulo","Brazil","AC Milan","Italy",3L,2L,"toyota93.html"),
  list("1994","1994-12-11","Vélez Sarsfield","Argentina","AC Milan","Italy",2L,0L,"toyota94.html"),
  list("1995","1995-11-28","Ajax","Netherlands","Grêmio","Brazil",0L,0L,"toyota95.html"),
  list("1996","1996-11-26","Juventus","Italy","River Plate","Argentina",1L,0L,"toyota96.html"),
  list("1997","1997-12-02","Borussia Dortmund","Germany","Cruzeiro","Brazil",2L,0L,"toyota97.html"),
  list("1998","1998-12-01","Real Madrid","Spain","Vasco da Gama","Brazil",2L,1L,"toyota98.html"),
  list("1999","1999-11-30","Manchester United","England","Palmeiras","Brazil",1L,0L,"toyota99.html"),
  list("2000","2000-11-28","Boca Juniors","Argentina","Real Madrid","Spain",2L,1L,"toyota00.html"),
  list("2001","2001-11-27","Bayern Munich","Germany","Boca Juniors","Argentina",1L,0L,"toyota01.html"),
  list("2002","2002-12-03","Real Madrid","Spain","Olimpia","Paraguay",2L,0L,"toyota02.html"),
  list("2003","2003-12-14","Boca Juniors","Argentina","AC Milan","Italy",1L,1L,"toyota03.html"),
  list("2004","2004-12-12","Porto","Portugal","Once Caldas","Colombia",0L,0L,"toyota04.html")
), use.names = FALSE)
setnames(toyota, c("Season","Date","Home","HomeAssociation","Away","AwayAssociation","HG","AG","Page"))
toyota[, `:=`(Stage = "Final", Era = "Toyota Cup")]

modern <- data.table::rbindlist(list(
  list("2024","2024-09-22","Ain","United Arab Emirates","Auckland City","New Zealand",6L,2L,"Preliminary Round"),
  list("2024","2024-10-29","Ahly","Egypt","Ain","United Arab Emirates",3L,0L,"Round 1"),
  list("2024","2024-12-11","Botafogo","Brazil","Pachuca","Mexico",0L,3L,"Round 1"),
  list("2024","2024-12-14","Pachuca","Mexico","Ahly","Egypt",0L,0L,"Final Qualifier"),
  list("2024","2024-12-18","Real Madrid","Spain","Pachuca","Mexico",3L,0L,"Final"),
  list("2025","2025-09-14","Pyramids","Egypt","Auckland City","New Zealand",3L,0L,"Preliminary Round"),
  list("2025","2025-09-23","Ahly","Saudi Arabia","Pyramids","Egypt",1L,3L,"Round 1"),
  list("2025","2025-12-10","Cruz Azul","Mexico","Flamengo","Brazil",1L,2L,"Round 1"),
  list("2025","2025-12-13","Flamengo","Brazil","Pyramids","Egypt",2L,0L,"Final Qualifier"),
  list("2025","2025-12-17","Paris Saint-Germain","France","Flamengo","Brazil",1L,1L,"Final")
), use.names = FALSE)
setnames(modern, c("Season","Date","Home","HomeAssociation","Away","AwayAssociation","HG","AG","Stage"))
modern[, `:=`(Page = paste0("fifa-icc", Season, ".html"), Era = "FIFA Intercontinental Cup")]

matches <- rbindlist(list(toyota, modern), use.names = TRUE)
matches[, Date := as.IDate(Date)]
matches[, `:=`(
  Country = "World",
  Competition = "fifa_intercontinental_cup",
  CompetitionType = "continental",
  Tier = NA_integer_,
  League = "FIFA Intercontinental Cup",
  Result = fifelse(HG > AG, "1-0", fifelse(HG < AG, "0-1", "0.5-0.5")),
  Score = paste0(HG, "-", AG),
  Source = "rsssf",
  SourcePage = Page,
  DateApprox = FALSE,
  SourceFile = fifelse(
    Era == "Toyota Cup",
    file.path(rsssf_root, "tablest", Page),
    file.path(rsssf_root, "tablesf", Page)
  )
)]

if (nrow(matches) != 35L || uniqueN(matches, by = c("Date","Home","Away","Score")) != 35L) {
  stop("Intercontinental Cup source table failed its 35-match uniqueness check.")
}
missing_pages <- unique(matches[!file.exists(SourceFile), SourceFile])
if (length(missing_pages)) stop("Cached RSSSF source page(s) missing: ", paste(missing_pages, collapse = ", "))

# Confirm every explicit identity currently exists in Elo. This detects a name
# mismatch before the master is changed.
ratings <- fread(ratings_path, encoding = "UTF-8")
rating_keys <- unique(paste(ratings$Country, ratings$Team, sep = "\r"))
matches[, `:=`(
  HomeKnown = paste(HomeAssociation, Home, sep = "\r") %chin% rating_keys,
  AwayKnown = paste(AwayAssociation, Away, sep = "\r") %chin% rating_keys
)]
unknown <- unique(rbindlist(list(
  matches[HomeKnown == FALSE, .(Association = HomeAssociation, Team = Home)],
  matches[AwayKnown == FALSE, .(Association = AwayAssociation, Team = Away)]
)))
if (nrow(unknown)) {
  fwrite(unknown, file.path(audit_dir, "unresolved_team_identities.csv"))
  stop("Unresolved team identities found; review unresolved_team_identities.csv. Master unchanged.")
}

first_dates <- ratings[, .(Association = Country, Team, FirstMatchDate = as.IDate(FirstMatchDate))]
matches[first_dates, on = .(HomeAssociation = Association, Home = Team), HomeFirstDate := i.FirstMatchDate]
matches[first_dates, on = .(AwayAssociation = Association, Away = Team), AwayFirstDate := i.FirstMatchDate]
matches[, EloEligibleNow := !is.na(HomeFirstDate) & !is.na(AwayFirstDate) &
          Date >= HomeFirstDate & Date >= AwayFirstDate]

audit <- matches[, .(
  Era, Season, Date, Stage, HomeAssociation, Home, AwayAssociation, Away,
  Score, SourcePage, EloEligibleNow, HomeFirstDate, AwayFirstDate
)]
fwrite(audit, file.path(audit_dir, "import_audit.csv"), na = "")

cat("\nIntercontinental Cup import audit:\n")
print(matches[, .(
  Matches = .N,
  EloEligibleNow = sum(EloEligibleNow),
  FirstDate = min(Date),
  LastDate = max(Date)
), by = Era])

master <- fread(master_path, encoding = "UTF-8")
master_cols <- names(master)
replacement <- matches[, .(
  Season, Country, Competition, CompetitionType, Tier, League, Date, Home, Away,
  Result, Score, Source, SourcePage, Stage, DateApprox, SourceFile,
  HomeAssociation, AwayAssociation
)]
for (nm in setdiff(master_cols, names(replacement))) replacement[, (nm) := NA]
replacement <- replacement[, ..master_cols]

retained <- master[Competition != "fifa_intercontinental_cup"]
result <- rbindlist(list(retained, replacement), use.names = TRUE)
setorder(result, Date, Country, Home, Away)

if (identical(Sys.getenv("ICC_IMPORT_DRY_RUN"), "1")) {
  message("Dry run: production master unchanged.")
} else {
  stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  backup <- sub("[.]csv$", paste0("_before_intercontinental_cup_", stamp, ".csv"), master_path)
  if (!file.copy(master_path, backup)) stop("Could not create master backup.")
  tmp <- paste0(master_path, ".tmp")
  fwrite(result, tmp, na = "")
  check <- fread(tmp, encoding = "UTF-8")
  actual <- check[Competition == "fifa_intercontinental_cup", .N]
  if (nrow(check) != nrow(result) || actual != 35L) {
    unlink(tmp)
    stop("Written-file verification failed; production master unchanged. Backup: ", backup)
  }
  if (!file.copy(tmp, master_path, overwrite = TRUE)) {
    unlink(tmp)
    stop("Could not replace production master. Backup: ", backup)
  }
  unlink(tmp)
  message("Production master updated with 35 Intercontinental Cup matches.")
  message("Backup: ", backup)
  message("Next run 02_calculate_elo.R, then 03_write_json.R.")
}
