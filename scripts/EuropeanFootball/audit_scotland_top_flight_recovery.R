# Review-only recovery audit for the three incomplete modern Scottish
# top-flight seasons. Uses cached RSSSF pages; no downloads or master changes.
suppressPackageStartupMessages(library(data.table))

root <- normalizePath(Sys.getenv("J_RATINGS_REPO",getwd()),winslash="/",mustWork=TRUE)
base <- file.path(root,"EuropeanFootball/pipeline_data")
out <- file.path(base,"Manual_Sources/RSSSF_Scotland_Top_Flight_Recovery")
staging <- file.path(base,"Source/rsssf/all/review/pages/scotland_top_flight_recovery")
dir.create(out,recursive=TRUE,showWarnings=FALSE)
dir.create(staging,recursive=TRUE,showWarnings=FALSE)

source_candidates <- list(
  scot2015.html=c(file.path(base,"Source/rsssf/all/pages/tabless/scot2015.html")),
  scot2019.html=c(file.path(base,"Source/rsssf/all/review/pages/tabless/scot2019.html"),
                  file.path(base,"Source/rsssf/uefa_raw/pages/tabless/scot2019.html")),
  scot2021.html=c(file.path(base,"Source/rsssf/all/pages/tabless/scot2021.html"))
)
manifest <- rbindlist(lapply(names(source_candidates),function(filename) {
  found <- source_candidates[[filename]][file.exists(source_candidates[[filename]])]
  data.table(Filename=filename,SourceFile=if(length(found)) normalizePath(found[1L],winslash="/") else NA_character_)
}))
if(anyNA(manifest$SourceFile)) stop("Missing cached Scotland pages: ",paste(manifest[is.na(SourceFile),Filename],collapse=", "))
manifest[,StagedFile:=file.path(staging,Filename)]
for(i in seq_len(nrow(manifest))) file.copy(manifest$SourceFile[i],manifest$StagedFile[i],overwrite=TRUE)
fwrite(manifest,file.path(out,"source_manifest.csv"),na="")
message("Prepared 3 cached Scottish top-flight pages. Downloads: 0.")

Sys.setenv(RSSSF_OFC_FUNCTIONS_ONLY="1")
source(file.path(root,"scripts/EuropeanFootball/rsssf_ofc_audit.R"),encoding="UTF-8")
Sys.unsetenv("RSSSF_OFC_FUNCTIONS_ONLY")
Sys.setenv(SCOTLAND_RECOVERY_MIN_YEAR="2015",SCOTLAND_RECOVERY_MAX_YEAR="2021",
           SCOTLAND_RECOVERY_RESUME="1")
cfg <- data.table(Country="Scotland",Confederation="UEFA",
  Directory="scotland_top_flight_recovery",
  FilePattern="^scot(?:2015|2019|2021)[.]html$",
  Competition="Scotland - Premiership",CompetitionType="league")
parsed <- run_rsssf_ofc_audit(config_override=cfg,audit_name="Scotland top-flight recovery",
  output_folder="RSSSF_Scotland_Top_Flight_Recovery/parser",env_prefix="SCOTLAND_RECOVERY")

games <- copy(parsed$games)
games <- games[!is.na(Date) & !(tolower(as.character(Annotated)) %chin% c("true","t","1"))]
games[,Date:=as.IDate(Date)]
# RSSSF appends the six promotion/relegation playoff matches to this block.
# Retain the 198 regular matches and the 30 split matches by requiring both
# clubs to have participated in the Premiership regular phase that season.
games <- games[, {
  top_clubs <- unique(c(Home[RSSSFPhase=="regular"],Away[RSSSFPhase=="regular"]))
  .SD[Home %chin% top_clubs & Away %chin% top_clubs]
}, by=Season]
aliases <- fread(file.path(base,"Reference/team_aliases.csv"),encoding="UTF-8")
aliases <- unique(aliases[Country=="Scotland" & nzchar(SourceName) & nzchar(CanonicalName),
                          .(SourceName,CanonicalName)])
alias_map <- setNames(aliases$CanonicalName,aliases$SourceName)
alias_map <- c(alias_map,
  "Saint Johnstone"="St. Johnstone",
  "Saint Mirren"="St. Mirren",
  "Inverness CT"="Inverness Caledonian Thistle FC")
canonical <- function(x) { hit<-unname(alias_map[x]); x[!is.na(hit)]<-hit[!is.na(hit)]; x }
games[,`:=`(CanonicalHome=canonical(Home),CanonicalAway=canonical(Away))]

master <- fread(file.path(base,"Matches_Clean_Combined/european_football_all_matches.csv"),encoding="UTF-8")
master <- master[Country=="Scotland" & CompetitionType=="league" & as.integer(Tier)==1L]
master[,`:=`(Date=as.IDate(Date),CanonicalHome=canonical(Home),CanonicalAway=canonical(Away))]
norm <- function(x) tolower(iconv(x,to="ASCII//TRANSLIT"))
key <- function(date,home,away,score) paste(date,norm(home),norm(away),score,sep="\r")
games[,FixtureKey:=key(Date,CanonicalHome,CanonicalAway,Score)]
master[,FixtureKey:=key(Date,CanonicalHome,CanonicalAway,Score)]
games[,InMaster:=FixtureKey %chin% master$FixtureKey]
nodate_key <- function(season,home,away,score) paste(season,norm(home),norm(away),score,sep="\r")
games[,NoDateKey:=nodate_key(Season,CanonicalHome,CanonicalAway,Score)]
master[,NoDateKey:=nodate_key(Season,CanonicalHome,CanonicalAway,Score)]
master_dates_by_fixture <- split(master$Date,master$NoDateKey)
games[,SameFixtureDifferentDate:=vapply(seq_len(.N),function(i) {
  if(InMaster[i]) return(FALSE)
  known_dates <- master_dates_by_fixture[[NoDateKey[i]]]
  length(known_dates)>0L && any(abs(as.integer(Date[i]-known_dates))<=1L)
},logical(1L))]

missing <- unique(games[InMaster==FALSE & SameFixtureDifferentDate==FALSE],by="FixtureKey")
missing[,`:=`(Home=CanonicalHome,Away=CanonicalAway)]
comparison <- merge(
  parsed$audit[,.(Season,RSSSFPlayedResults,RSSSFDatedResults,DatedShareOfRSSSFPercent,
                   QualityStatus,SourceAssessment,SourceFile,Error)],
  master[,.(MasterMatches=.N),by=Season],by="Season",all.x=TRUE)
comparison <- merge(comparison,games[,.(ParsedDated=.N,AlreadyInMaster=sum(InMaster),
    DifferentDateReview=sum(SameFixtureDifferentDate),
    RecoverableMissing=sum(InMaster==FALSE & SameFixtureDifferentDate==FALSE)),by=Season],
                    by="Season",all.x=TRUE)
for(col in c("MasterMatches","ParsedDated","AlreadyInMaster","DifferentDateReview","RecoverableMissing"))
  comparison[is.na(get(col)),(col):=0L]
setorder(comparison,Season)
fwrite(comparison,file.path(out,"season_comparison.csv"),na="")
fwrite(missing[,.(Season,Date,Home,Away,Score,Result,SourcePage,SourceFile,SourceLine,RawLine)],
       file.path(out,"recoverable_missing_matches.csv"),na="")
print(comparison[,.(Season,MasterMatches,ParsedDated,AlreadyInMaster,DifferentDateReview,RecoverableMissing,QualityStatus,Error)])
message("Recoverable dated matches not currently in master: ",nrow(missing),".")
message("Review: ",file.path(out,"season_comparison.csv"))
message("Candidate matches: ",file.path(out,"recoverable_missing_matches.csv"))
message("Production master unchanged.")
