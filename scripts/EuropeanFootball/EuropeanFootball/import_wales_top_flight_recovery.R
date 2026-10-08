# Add only the dated Welsh top-flight fixtures proven absent by the recovery
# audit. Safe to rerun: canonical fixture keys prevent duplicate insertion.
# Preview is the default. Set APPLY_WALES_TOP_FLIGHT_RECOVERY=1 to write.
suppressPackageStartupMessages(library(data.table))
root <- normalizePath(Sys.getenv("J_RATINGS_REPO",getwd()),winslash="/",mustWork=TRUE)
base <- file.path(root,"EuropeanFootball/pipeline_data")
review <- file.path(base,"Manual_Sources/RSSSF_Wales_Top_Flight_Recovery")
candidate_path <- file.path(review,"recoverable_missing_matches.csv")
master_path <- file.path(base,"Matches_Clean_Combined/european_football_all_matches.csv")
alias_path <- file.path(base,"Reference/team_aliases.csv")
stopifnot(file.exists(candidate_path),file.exists(master_path),file.exists(alias_path))

candidates <- fread(candidate_path,encoding="UTF-8")
master <- fread(master_path,encoding="UTF-8")
aliases <- fread(alias_path,encoding="UTF-8")
master_columns <- names(master)
candidates[,Date:=as.IDate(Date)]
if(anyNA(candidates$Date) || any(!candidates$Result %chin% c("1-0","0.5-0.5","0-1")))
  stop("Recovery candidate contains an invalid date or result.")
if(candidates[Season=="2001/02",.N]!=306L)
  stop("Expected all 306 dated 2001/02 fixtures in the recovery batch.")

extra_aliases <- data.table(Country="Wales",
  SourceName=c("Aberytswyth","Cwmbran´","Rhayader","Rhayader T","Oswestry","Conwy","Llansantffraid","Inter Cardiff"),
  CanonicalName=c("Aberystwyth Town","Cwmbran Town","Rhayader Town","Rhayader Town","Oswestry Town","Conwy Borough","The New Saints","Cardiff Metropolitan University"))
all_aliases <- unique(rbindlist(list(aliases,extra_aliases),use.names=TRUE,fill=TRUE),
                      by=c("Country","SourceName"))
wales_aliases <- all_aliases[Country=="Wales"]
alias_map <- setNames(wales_aliases$CanonicalName,wales_aliases$SourceName)
canonical <- function(x) { hit<-unname(alias_map[x]); x[!is.na(hit)]<-hit[!is.na(hit)]; x }

master_wales <- master[Country=="Wales" & CompetitionType=="league" & as.integer(Tier)==1L]
master_wales[,`:=`(Date=as.IDate(Date),CanonicalHome=canonical(Home),CanonicalAway=canonical(Away))]
candidates[,`:=`(Home=canonical(Home),Away=canonical(Away))]
norm <- function(x) tolower(iconv(x,to="ASCII//TRANSLIT"))
key <- function(date,home,away,score) paste(date,norm(home),norm(away),score,sep="\r")
pair_key <- function(date,home,away) paste(date,norm(home),norm(away),sep="\r")
candidates[,FixtureKey:=key(Date,Home,Away,Score)]
master_wales[,FixtureKey:=key(Date,CanonicalHome,CanonicalAway,Score)]
candidates <- unique(candidates[!FixtureKey %chin% master_wales$FixtureKey],by="FixtureKey")
if(anyDuplicated(candidates$FixtureKey)) stop("Recovery batch contains duplicate fixture keys.")

# A same-day home/away pairing with a different score is a genuine conflict.
candidate_pairs <- pair_key(candidates$Date,candidates$Home,candidates$Away)
master_pairs <- pair_key(master_wales$Date,master_wales$CanonicalHome,master_wales$CanonicalAway)
pair_hits <- match(candidate_pairs,master_pairs)
if(any(!is.na(pair_hits) & candidates$Score!=master_wales$Score[pair_hits]))
  stop("A recovered fixture conflicts with an existing same-day score.")

# No club may be placed in another Welsh league match on the same date.
master_team_dates <- unique(c(paste(master_wales$Date,norm(master_wales$CanonicalHome),sep="\r"),
                              paste(master_wales$Date,norm(master_wales$CanonicalAway),sep="\r")))
candidate_team_dates <- c(paste(candidates$Date,norm(candidates$Home),sep="\r"),
                          paste(candidates$Date,norm(candidates$Away),sep="\r"))
if(any(candidate_team_dates %chin% master_team_dates))
  stop("A recovered fixture would double-book a club on an existing match date.")

new_rows <- data.table(Season=candidates$Season,Country="Wales",Competition="Cymru Premier",
  CompetitionType="league",Tier=1L,League="Cymru Premier",Date=candidates$Date,
  Home=candidates$Home,Away=candidates$Away,Result=candidates$Result,Score=candidates$Score,
  Source="rsssf",SourcePage=candidates$SourcePage,Stage="Recovered RSSSF top-flight fixture",
  DateApprox=FALSE,SourceFile=candidates$SourceFile,HomeAssociation="Wales",AwayAssociation="Wales")
for(column in setdiff(master_columns,names(new_rows))) new_rows[,(column):=NA]
new_rows <- new_rows[,..master_columns]
summary <- new_rows[,.(AddedMatches=.N,FirstDate=min(as.IDate(Date)),LastDate=max(as.IDate(Date))),by=Season][order(Season)]
print(summary)
message("Validated Welsh top-flight additions: ",nrow(new_rows),".")
if(!identical(Sys.getenv("APPLY_WALES_TOP_FLIGHT_RECOVERY"),"1")) {
  message("Preview only. Production master and alias file unchanged.")
} else {
  result <- rbindlist(list(master,new_rows),use.names=TRUE,fill=FALSE)
  setorder(result,Date,Country,Tier,Home,Away)
  all_keys <- result[,paste(Season,Country,Date,Home,Away,Score,sep="\r")]
  if(anyDuplicated(all_keys)) stop("Proposed master contains an exact duplicate fixture.")
  stamp <- format(Sys.time(),"%Y%m%d_%H%M%S")
  master_backup <- sub("[.]csv$",paste0("_before_wales_top_flight_recovery_",stamp,".csv"),master_path)
  alias_backup <- sub("[.]csv$",paste0("_before_wales_top_flight_recovery_",stamp,".csv"),alias_path)
  if(!file.copy(master_path,master_backup,overwrite=FALSE) || !file.copy(alias_path,alias_backup,overwrite=FALSE))
    stop("Could not create recovery backups.")
  fwrite(result,master_path,na="")
  fwrite(all_aliases,alias_path,na="")
  fwrite(summary,file.path(review,paste0("applied_import_",stamp,".csv")),na="")
  message("Production master updated: ",nrow(master)," -> ",nrow(result)," rows.")
  message("Master backup: ",master_backup)
  message("Alias backup: ",alias_backup)
}
