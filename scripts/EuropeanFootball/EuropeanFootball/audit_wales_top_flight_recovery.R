# Audit every available Welsh national top-flight RSSSF page against the
# production master. This script is review-only: it never changes production.
suppressPackageStartupMessages(library(data.table))

root <- normalizePath(Sys.getenv("J_RATINGS_REPO", getwd()), winslash="/", mustWork=TRUE)
base <- file.path(root,"EuropeanFootball/pipeline_data")
out <- file.path(base,"Manual_Sources/RSSSF_Wales_Top_Flight_Recovery")
staging <- file.path(base,"Source/rsssf/all/review/pages/wales_top_flight_recovery")
dir.create(out,recursive=TRUE,showWarnings=FALSE)
dir.create(staging,recursive=TRUE,showWarnings=FALSE)

page_name <- function(end_year) {
  if (end_year < 2000L) sprintf("wal%02d.html",end_year %% 100L) else
    if (end_year < 2010L) sprintf("wal%02d.html",end_year %% 100L) else
      sprintf("wal%d.html",end_year)
}

source_roots <- c(
  file.path(base,"Manual_Sources/RSSSF_Gap_Recovery/pages/tablesw"),
  file.path(base,"Source/rsssf/all/pages/tablesw")
)
wiki_cache <- file.path(base,"Manual_Sources/Wikipedia_RSSSF_Remaining_UEFA_Leagues/cache")

manifest <- rbindlist(lapply(1993:2025,function(end_year) {
  filename <- page_name(end_year)
  direct <- file.path(source_roots,filename)
  encoded <- file.path(wiki_cache,paste0("https___www.rsssf.org_tablesw_",filename,".html"))
  candidates <- c(direct,encoded)
  candidates <- candidates[file.exists(candidates)]
  data.table(Season=sprintf("%d/%02d",end_year-1L,end_year%%100L),EndYear=end_year,
    Filename=filename,SourceFile=if(length(candidates)) normalizePath(candidates[1L],winslash="/") else NA_character_)
}))
manifest[,StagedFile:=file.path(staging,Filename)]
for(i in seq_len(nrow(manifest))) if(!is.na(manifest$SourceFile[i]))
  file.copy(manifest$SourceFile[i],manifest$StagedFile[i],overwrite=TRUE)
manifest[,Available:=file.exists(StagedFile)]
fwrite(manifest,file.path(out,"source_manifest.csv"),na="")
message("Welsh top-flight pages available: ",sum(manifest$Available),"/",nrow(manifest),".")
if(any(!manifest$Available)) message("Missing cached pages: ",paste(manifest[!Available,Filename],collapse=", "))

Sys.setenv(RSSSF_OFC_FUNCTIONS_ONLY="1")
source(file.path(root,"scripts/EuropeanFootball/rsssf_ofc_audit.R"),encoding="UTF-8")
Sys.unsetenv("RSSSF_OFC_FUNCTIONS_ONLY")
Sys.setenv(WALES_RECOVERY_MIN_YEAR="1993",WALES_RECOVERY_MAX_YEAR="2025",WALES_RECOVERY_RESUME="1")
cfg <- data.table(Country="Wales",Confederation="UEFA",
  Directory="wales_top_flight_recovery",
  FilePattern="^wal(?:9[3-9]|[0-9]{2}|20[0-9]{2})[.]html$",
  Competition="Wales - Premier",CompetitionType="league")
parsed <- run_rsssf_ofc_audit(config_override=cfg,audit_name="Wales top-flight recovery",
  output_folder="RSSSF_Wales_Top_Flight_Recovery/parser",env_prefix="WALES_RECOVERY")

games <- copy(parsed$games)
games <- games[!is.na(Date) & !(as.character(Annotated) %chin% c("TRUE","T","1"))]
games[,Date:=as.IDate(Date)]
aliases <- fread(file.path(base,"Reference/team_aliases.csv"),encoding="UTF-8")
aliases <- unique(aliases[Country=="Wales" & nzchar(SourceName) & nzchar(CanonicalName),
                          .(SourceName,CanonicalName)])
alias_map <- setNames(aliases$CanonicalName,aliases$SourceName)
# Obvious historical abbreviations/typos present in these specific Welsh pages.
# Each target is a unique club identity; none is inferred by substring alone.
alias_map <- c(alias_map,
  "Aberytswyth"="Aberystwyth Town",
  "Cwmbran´"="Cwmbran Town",
  "Rhayader"="Rhayader Town",
  "Rhayader T"="Rhayader Town",
  "Oswestry"="Oswestry Town",
  "Conwy"="Conwy Borough",
  "Llansantffraid"="The New Saints",
  "Inter Cardiff"="Cardiff Metropolitan University")
canonical <- function(x) {
  hit <- unname(alias_map[x]); x[!is.na(hit)] <- hit[!is.na(hit)]; x
}
games[,`:=`(CanonicalHome=canonical(Home),CanonicalAway=canonical(Away))]

master_path <- file.path(base,"Matches_Clean_Combined/european_football_all_matches.csv")
master <- fread(master_path,encoding="UTF-8")
master <- master[Country=="Wales" & CompetitionType=="league" & as.integer(Tier)==1L]
master[,Date:=as.IDate(Date)]
master[,`:=`(CanonicalHome=canonical(Home),CanonicalAway=canonical(Away))]

key <- function(date,home,away,score) paste(date,tolower(iconv(home,to="ASCII//TRANSLIT")),
  tolower(iconv(away,to="ASCII//TRANSLIT")),score,sep="\r")
games[,FixtureKey:=key(Date,CanonicalHome,CanonicalAway,Score)]
master[,FixtureKey:=key(Date,CanonicalHome,CanonicalAway,Score)]
master_keys <- unique(master$FixtureKey)
games[,InMaster:=FixtureKey %chin% master_keys]
nodate_key <- function(season,home,away,score) paste(season,
  tolower(iconv(home,to="ASCII//TRANSLIT")),tolower(iconv(away,to="ASCII//TRANSLIT")),score,sep="\r")
games[,NoDateKey:=nodate_key(Season,CanonicalHome,CanonicalAway,Score)]
master[,NoDateKey:=nodate_key(Season,CanonicalHome,CanonicalAway,Score)]
master_nodate_keys <- unique(master$NoDateKey)
games[,SameFixtureDifferentDate:=InMaster==FALSE & NoDateKey %chin% master_nodate_keys]

missing <- unique(games[InMaster==FALSE],by="FixtureKey")
date_conflicts <- unique(games[SameFixtureDifferentDate==TRUE],by="FixtureKey")
missing[,`:=`(Home=CanonicalHome,Away=CanonicalAway)]
comparison <- merge(
  parsed$audit[,.(Season,RSSSFPlayedResults,RSSSFDatedResults,DatedShareOfRSSSFPercent,
                   QualityStatus,SourceAssessment,SourceFile,Error)],
  master[,.(MasterMatches=.N),by=Season],by="Season",all.x=TRUE)
comparison <- merge(comparison,games[,.(ParsedDated=.N,AlreadyInMaster=sum(InMaster),
    DifferentDateReview=sum(SameFixtureDifferentDate),
    RecoverableMissing=sum(InMaster==FALSE)),by=Season],
                    by="Season",all.x=TRUE)
for(col in c("MasterMatches","ParsedDated","AlreadyInMaster","DifferentDateReview","RecoverableMissing"))
  comparison[is.na(get(col)),(col):=0L]
setorder(comparison,Season)

fwrite(comparison,file.path(out,"season_comparison.csv"),na="")
fwrite(missing[,.(Season,Date,Home,Away,Score,Result,SourcePage,SourceFile,SourceLine,RawLine)],
       file.path(out,"recoverable_missing_matches.csv"),na="")
fwrite(date_conflicts[,.(Season,Date,CanonicalHome,CanonicalAway,Score,SourcePage,SourceLine,RawLine)],
       file.path(out,"different_date_review.csv"),na="")
print(comparison[,.(Season,MasterMatches,RSSSFDatedResults,AlreadyInMaster,DifferentDateReview,RecoverableMissing,QualityStatus,Error)])
message("Recoverable dated matches not currently in master: ",nrow(missing),".")
message("Review: ",file.path(out,"season_comparison.csv"))
message("Candidate matches: ",file.path(out,"recoverable_missing_matches.csv"))
message("Production master unchanged.")
