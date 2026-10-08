# Wikipedia results + RSSSF dates: audit the 33 UEFA leagues not yet imported.
# Run from anywhere:
# source("C:/Users/stjuk/Documents/GitHub/J-Ratings/scripts/EuropeanFootball/wikipedia_rsssf_remaining_uefa_leagues.R")
# Optional filters (START years; defaults: 1900 through the current year):
# Sys.setenv(UEFA_AUDIT_COUNTRIES="Croatia,Serbia", UEFA_AUDIT_MIN_YEAR=2012,
#            UEFA_AUDIT_MAX_YEAR=2024)
# Clear filters for the full historical audit:
# Sys.unsetenv(c("UEFA_AUDIT_COUNTRIES","UEFA_AUDIT_MIN_YEAR","UEFA_AUDIT_MAX_YEAR"))
# install.packages(c("data.table","xml2","rvest","stringi","tictoc","beepr"))
# Only audit/candidate files are written. No production import or Elo calculation.
# Reruns default to cached pages only: no download waits or failed-URL retries.
# To allow missing pages to download: Sys.setenv(UEFA_AUDIT_CACHE_ONLY="0")
# Uncached downloads retain Sys.sleep(1) in the shared fetch function.
# Coverage is the share of EXTRACTED Wikipedia games matched to an RSSSF date;
# it does not prove Wikipedia includes every fixture in the season.

run_remaining_uefa_audit <- function() {
  required <- c("data.table","xml2","rvest","stringi","tictoc","beepr")
  missing <- required[!vapply(required,requireNamespace,logical(1),quietly=TRUE)]
  if(length(missing)) stop("Install these packages first: ",paste(missing,collapse=", "))
  on.exit(beepr::beep(),add=TRUE)
  root <- normalizePath(Sys.getenv("J_RATINGS_REPO",
    "C:/Users/stjuk/Documents/GitHub/J-Ratings"),winslash="/",mustWork=TRUE)
  # Load the shared parser without starting the four-country rebuild. Restore
  # the caller's environment setting even if loading fails.
  previous <- Sys.getenv("FOUR_LEAGUES_FUNCTIONS_ONLY",unset=NA_character_)
  on.exit({if(is.na(previous)) Sys.unsetenv("FOUR_LEAGUES_FUNCTIONS_ONLY") else
    Sys.setenv(FOUR_LEAGUES_FUNCTIONS_ONLY=previous)},add=TRUE)
  Sys.setenv(FOUR_LEAGUES_FUNCTIONS_ONLY="1")
  previous_cache <- Sys.getenv("UEFA_AUDIT_CACHE_ONLY",unset=NA_character_)
  if(is.na(previous_cache)) {
    Sys.setenv(UEFA_AUDIT_CACHE_ONLY="1")
    on.exit(Sys.unsetenv("UEFA_AUDIT_CACHE_ONLY"),add=TRUE)
  }
  engine <- new.env(parent=globalenv())
  sys.source(file.path(root,"scripts/EuropeanFootball/wikipedia_rsssf_four_leagues.R"),envir=engine)

  # Alternative Wikipedia competition names cover renamings in older seasons.
  # Calendar describes the usual current format; the RSSSF title determines
  # each actual season, including historical changes of calendar.
  config <- data.table::fread(text="Country;Prefix;League;Calendar;WikiNames
Albania;tablesa/alba;Kategoria Superiore;FALSE;Kategoria Superiore|Albanian Superliga|Albanian National Championship
Andorra;tablesa/ando;Primera Divisio;FALSE;Primera Divisió
Armenia;tablesa/arme;Armenian Premier League;FALSE;Armenian Premier League
Azerbaijan;tablesa/azer;Azerbaijan Premier League;FALSE;Azerbaijan Premier League|Azerbaijan Top League
Belarus;tablesw/witr;Belarusian Premier League;TRUE;Belarusian Premier League
Bosnia and Herzegovina;tablesb/bih;Premier League of Bosnia and Herzegovina;FALSE;Premier League of Bosnia and Herzegovina|First League of Bosnia and Herzegovina
Bulgaria;tablesb/bulg;Bulgarian First League;FALSE;First Professional Football League (Bulgaria)|First League (Bulgaria)|A Group|Bulgarian A Group
Croatia;tablesk/kroa;Croatian Football League;FALSE;Croatian Football League|Croatian First Football League
Cyprus;tablesc/cyp;Cypriot First Division;FALSE;Cypriot First Division
Estonia;tablese/est;Meistriliiga;TRUE;Meistriliiga
Faroe Islands;tablesf/far;Faroe Islands Premier League;TRUE;Faroe Islands Premier League|Faroe Islands Premier League football|1. deild
Finland;tablesf/fin;Veikkausliiga;TRUE;Veikkausliiga|Mestaruussarja
Georgia;tablesg/geor;Erovnuli Liga;TRUE;Erovnuli Liga|Umaglesi Liga
Gibraltar;tablesg/gib;Gibraltar Football League;FALSE;Gibraltar Football League|Gibraltar National League|Gibraltar Premier Division
Hungary;tablesh/hong;Nemzeti Bajnoksag I;FALSE;Nemzeti Bajnokság I
Iceland;tablesi/ijs;Besta deild karla;TRUE;Besta deild karla|Úrvalsdeild|Úrvalsdeild karla|1. deild karla
Israel;tablesi/isra;Israeli Premier League;FALSE;Israeli Premier League|Liga Leumit|Liga Alef
Kazakhstan;tablesk/kaz;Kazakhstan Premier League;TRUE;Kazakhstan Premier League|Kazakhstan Top Division
Kosovo;tablesk/kosovo;Football Superleague of Kosovo;FALSE;Football Superleague of Kosovo|Superleague of Kosovo
Latvia;tablesl/let;Latvian Higher League;TRUE;Latvian Higher League
Lithuania;tablesl/lito;A Lyga;TRUE;A Lyga|LFF Lyga|Lithuanian A Lyga
Luxembourg;tablesl/lux;Luxembourg National Division;FALSE;Luxembourg National Division
Malta;tablesm/malt;Maltese Premier League;FALSE;Maltese Premier League|Maltese First Division
Moldova;tablesm/mold;Moldovan Super Liga;FALSE;Moldovan Super Liga|Moldovan National Division
Montenegro;tablesm/monteg;Montenegrin First League;FALSE;Montenegrin First League
North Macedonia;tablesn/nmkd;Macedonian First Football League;FALSE;Macedonian First Football League
Northern Ireland;tablesn/nil;NIFL Premiership;FALSE;NIFL Premiership|IFA Premiership|Irish Premier League|Irish League
Republic of Ireland;tablesi/ier;League of Ireland Premier Division;TRUE;League of Ireland Premier Division|League of Ireland
San Marino;tabless/sanm;Campionato Sammarinese;FALSE;Campionato Sammarinese di Calcio
Serbia;tabless/serv;Serbian SuperLiga;FALSE;Serbian SuperLiga|First League of Serbia and Montenegro|First League of FR Yugoslavia
Slovakia;tabless/slow;Slovak First Football League;FALSE;Slovak First Football League|Slovak Super Liga|Slovak Superliga
Slovenia;tabless/slov;Slovenian PrvaLiga;FALSE;Slovenian PrvaLiga
Wales;tablesw/wal;Cymru Premier;FALSE;Cymru Premier|Welsh Premier League|League of Wales",sep=";",quote="",encoding="UTF-8")
  config[,OldPrefixes:=""]
  config[Country=="North Macedonia",OldPrefixes:="tablesf/fyrom"]
  engine$run_four_leagues(config_override=config,
    output_name="Wikipedia_RSSSF_Remaining_UEFA_Leagues",
    env_prefix="UEFA_AUDIT",run_label="Wikipedia + RSSSF remaining UEFA league audit")
}

run_remaining_uefa_audit()
