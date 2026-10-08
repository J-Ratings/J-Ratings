# Wikipedia results + RSSSF dates: unified alias-review audit for UEFA leagues.
# This writes only audit/review files. It never changes the production match CSV.
#
# Run from anywhere:
# source("C:/Users/stjuk/Documents/GitHub/J-Ratings/scripts/EuropeanFootball/wikipedia_rsssf_alias_audit.R")
#
# Defaults to seasons through 2025 (the 2025/26 start year). To change that:
# Sys.setenv(ALIAS_AUDIT_MIN_YEAR=2000, ALIAS_AUDIT_MAX_YEAR=2025)
# To restrict countries while testing:
# Sys.setenv(ALIAS_AUDIT_COUNTRIES="Belarus,Finland,Gibraltar,Slovenia")
# Completed seasons resume by default. Before 2010, Wikipedia is cache-only.
# Failed URL attempts are remembered; ALIAS_AUDIT_RETRY_FAILED=1 retries them.
# Set ALIAS_AUDIT_RESUME=0 to recompute UNLOCKED seasons after parser changes.
# Seasons with raw coverage strictly above 95%, no extraction-review flag, and
# valid dated rows are copied to locked_seasons/ and skipped even with RESUME=0.
# locked_seasons.csv lists these protected outputs. These are coverage-based
# snapshots, not independent proof that Wikipedia contains every fixture.
# ALIAS_AUDIT_USE_LOCKS=0 bypasses snapshots for explicit diagnostic reruns;
# existing snapshots remain intact and are reused when USE_LOCKS returns to 1.
# Recommended offline rerun after parser changes:
# Sys.setenv(ALIAS_AUDIT_RESUME="0", ALIAS_AUDIT_USE_LOCKS="1", ALIAS_AUDIT_CACHE_ONLY="1")
# Allow missing pages from 2010 onward to download with:
# Sys.setenv(ALIAS_AUDIT_CACHE_ONLY="0")
# The shared parser waits 0.5 seconds before each uncached download (configurable
# with ALIAS_AUDIT_DOWNLOAD_DELAY). Step timings are appended to step_timings.csv.
# It includes
# the completed alias_review.csv report in the output directory.

run_alias_audit <- function() {
  required <- c("data.table","xml2","rvest","stringi","tictoc","beepr")
  missing <- required[!vapply(required,requireNamespace,logical(1),quietly=TRUE)]
  if(length(missing)) stop("Install these packages first: ",paste(missing,collapse=", "))
  on.exit(tryCatch(beepr::beep(),
    warning=function(w) message("Audit finished; completion sound unavailable: ",conditionMessage(w)),
    error=function(e) message("Audit finished; completion sound unavailable: ",conditionMessage(e))),add=TRUE)

  root <- normalizePath(Sys.getenv("J_RATINGS_REPO",
    "C:/Users/stjuk/Documents/GitHub/J-Ratings"),winslash="/",mustWork=TRUE)
  # Historical checkpoint: 2025 is the start year of 2025/26.
  if(!nzchar(Sys.getenv("ALIAS_AUDIT_MAX_YEAR"))) Sys.setenv(ALIAS_AUDIT_MAX_YEAR="2025")

  config <- data.table::fread(text="Country;Prefix;League;Calendar;WikiNames;OldPrefixes
Albania;tablesa/alba;Kategoria Superiore;FALSE;Kategoria Superiore|Albanian Superliga|Albanian National Championship;
Andorra;tablesa/ando;Primera Divisio;FALSE;Primera Divisió;
Armenia;tablesa/arme;Armenian Premier League;FALSE;Armenian Premier League;
Azerbaijan;tablesa/azer;Azerbaijan Premier League;FALSE;Azerbaijan Premier League|Azerbaijan Top League;
Belarus;tablesw/witr;Belarusian Premier League;TRUE;Belarusian Premier League;
Bosnia and Herzegovina;tablesb/bih;Premier League of Bosnia and Herzegovina;FALSE;Premier League of Bosnia and Herzegovina|First League of Bosnia and Herzegovina;
Bulgaria;tablesb/bulg;Bulgarian First League;FALSE;First Professional Football League (Bulgaria)|First League (Bulgaria)|A Group|Bulgarian A Group;
Croatia;tablesk/kroa;Croatian Football League;FALSE;Croatian Football League|Croatian First Football League;
Cyprus;tablesc/cyp;Cypriot First Division;FALSE;Cypriot First Division;
Estonia;tablese/est;Meistriliiga;TRUE;Meistriliiga;
Faroe Islands;tablesf/far;Faroe Islands Premier League;TRUE;Faroe Islands Premier League|Faroe Islands Premier League football|1. deild;
Finland;tablesf/fin;Veikkausliiga;TRUE;Veikkausliiga|Mestaruussarja;
Georgia;tablesg/geor;Erovnuli Liga;TRUE;Erovnuli Liga|Umaglesi Liga;
Gibraltar;tablesg/gib;Gibraltar Football League;FALSE;Gibraltar Football League|Gibraltar National League|Gibraltar Premier Division;
Hungary;tablesh/hong;Nemzeti Bajnoksag I;FALSE;Nemzeti Bajnokság I;
Iceland;tablesi/ijs;Besta deild karla;TRUE;Besta deild karla|Úrvalsdeild|Úrvalsdeild karla|1. deild karla;
Israel;tablesi/isra;Israeli Premier League;FALSE;Israeli Premier League|Liga Leumit|Liga Alef;
Kazakhstan;tablesk/kaz;Kazakhstan Premier League;TRUE;Kazakhstan Premier League|Kazakhstan Top Division;
Kosovo;tablesk/kosovo;Football Superleague of Kosovo;FALSE;Football Superleague of Kosovo|Superleague of Kosovo;
Latvia;tablesl/let;Latvian Higher League;TRUE;Latvian Higher League;
Lithuania;tablesl/lito;A Lyga;TRUE;A Lyga|LFF Lyga|Lithuanian A Lyga;
Luxembourg;tablesl/lux;Luxembourg National Division;FALSE;Luxembourg National Division;
Malta;tablesm/malt;Maltese Premier League;FALSE;Maltese Premier League|Maltese First Division;
Moldova;tablesm/mold;Moldovan Super Liga;FALSE;Moldovan Super Liga|Moldovan National Division;
Montenegro;tablesm/monteg;Montenegrin First League;FALSE;Montenegrin First League;
North Macedonia;tablesn/nmkd;Macedonian First Football League;FALSE;Macedonian First Football League;
Northern Ireland;tablesn/nil;NIFL Premiership;FALSE;NIFL Premiership|IFA Premiership|Irish Premier League|Irish League;
Republic of Ireland;tablesi/ier;League of Ireland Premier Division;TRUE;League of Ireland Premier Division|League of Ireland;
San Marino;tabless/sanm;Campionato Sammarinese;FALSE;Campionato Sammarinese di Calcio;
Serbia;tabless/serv;Serbian SuperLiga;FALSE;Serbian SuperLiga|First League of Serbia and Montenegro|First League of FR Yugoslavia;
Slovakia;tabless/slow;Slovak First Football League;FALSE;Slovak First Football League|Slovak Super Liga|Slovak Superliga;
Slovenia;tabless/slov;Slovenian PrvaLiga;FALSE;Slovenian PrvaLiga;
Wales;tablesw/wal;Cymru Premier;FALSE;Cymru Premier|Welsh Premier League|League of Wales;
Poland;tablesp/pol;Ekstraklasa;FALSE;Ekstraklasa|I liga;
Norway;tablesn/noo;Eliteserien;TRUE;Eliteserien|Tippeligaen|1. divisjon|Hovedserien|Norgesserien;
Sweden;tablesz/zwed;Allsvenskan;TRUE;Allsvenskan;
Romania;tablesr/roem;SuperLiga;FALSE;Liga I|Divizia A|Romanian football championship;
Russia;tablesr/rus;Russian Premier League;TRUE;Russian Premier League|Russian Football Premier League|Soviet Top League;",sep=";",quote="",encoding="UTF-8")
  config[,OldPrefixes:=ifelse(is.na(OldPrefixes),"",OldPrefixes)]
  config[Country=="North Macedonia",OldPrefixes:="tablesf/fyrom"]

  previous <- Sys.getenv("FOUR_LEAGUES_FUNCTIONS_ONLY",unset=NA_character_)
  on.exit({if(is.na(previous)) Sys.unsetenv("FOUR_LEAGUES_FUNCTIONS_ONLY") else
    Sys.setenv(FOUR_LEAGUES_FUNCTIONS_ONLY=previous)},add=TRUE)
  Sys.setenv(FOUR_LEAGUES_FUNCTIONS_ONLY="1")
  engine <- new.env(parent=globalenv())
  sys.source(file.path(root,"scripts/EuropeanFootball/wikipedia_rsssf_four_leagues.R"),envir=engine)
  engine$run_four_leagues(config_override=config,
    output_name="Wikipedia_RSSSF_Alias_Audit",env_prefix="ALIAS_AUDIT",
    run_label="Wikipedia + RSSSF unified UEFA alias audit")
  # Compare ordinary coverage with coverage excluding unresolved identities.
  # These reports never turn an unresolved game into a dated candidate.
  source(file.path(root,"scripts/EuropeanFootball/review_parser_coverage.R"),local=TRUE)
}

run_alias_audit()
