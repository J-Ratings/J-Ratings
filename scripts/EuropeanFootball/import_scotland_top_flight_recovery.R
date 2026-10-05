# Import the reviewed modern Scottish Premiership recovery batch.
# Set APPLY_SCOTLAND_TOP_FLIGHT_RECOVERY=1 to write; otherwise this previews.
suppressPackageStartupMessages(library(data.table))
root <- normalizePath(Sys.getenv("J_RATINGS_REPO",getwd()),winslash="/",mustWork=TRUE)
base <- file.path(root,"EuropeanFootball/pipeline_data")
review <- file.path(base,"Manual_Sources/RSSSF_Scotland_Top_Flight_Recovery")
candidate_path <- file.path(review,"recoverable_missing_matches.csv")
master_path <- file.path(base,"Matches_Clean_Combined/european_football_all_matches.csv")
alias_path <- file.path(base,"Reference/team_aliases.csv")
stopifnot(file.exists(candidate_path),file.exists(master_path),file.exists(alias_path))
candidates <- fread(candidate_path,encoding="UTF-8")
master <- fread(master_path,encoding="UTF-8")
aliases <- fread(alias_path,encoding="UTF-8")
master_columns <- names(master)
candidates[,Date:=as.IDate(Date)]
if(nrow(candidates)!=61L || candidates[Season=="2014/15",.N]!=1L ||
   candidates[Season=="2018/19",.N]!=30L || candidates[Season=="2020/21",.N]!=30L)
  stop("The reviewed Scotland batch is not the expected 1 + 30 + 30 matches.")
if(anyNA(candidates$Date) || any(!candidates$Result %chin% c("1-0","0.5-0.5","0-1")))
  stop("Recovery candidate contains an invalid date or result.")

extra_aliases <- data.table(Country="Scotland",
  SourceName=c("Saint Johnstone","Saint Mirren","Inverness CT"),
  CanonicalName=c("St. Johnstone","St. Mirren","Inverness Caledonian Thistle FC"))
all_aliases <- unique(rbindlist(list(aliases,extra_aliases),use.names=TRUE,fill=TRUE),
                      by=c("Country","SourceName"))
scotland_aliases <- all_aliases[Country=="Scotland"]
alias_map <- setNames(scotland_aliases$CanonicalName,scotland_aliases$SourceName)
canonical <- function(x) { hit<-unname(alias_map[x]); x[!is.na(hit)]<-hit[!is.na(hit)]; x }
candidates[,`:=`(Home=canonical(Home),Away=canonical(Away))]

master_scotland <- master[Country=="Scotland" & CompetitionType=="league" & as.integer(Tier)==1L]
master_scotland[,`:=`(Date=as.IDate(Date),CanonicalHome=canonical(Home),CanonicalAway=canonical(Away))]
norm <- function(x) tolower(iconv(x,to="ASCII//TRANSLIT"))
key <- function(date,home,away,score) paste(date,norm(home),norm(away),score,sep="\r")
candidates[,FixtureKey:=key(Date,Home,Away,Score)]
master_scotland[,FixtureKey:=key(Date,CanonicalHome,CanonicalAway,Score)]
if(any(candidates$FixtureKey %chin% master_scotland$FixtureKey))
  stop("A reviewed Scotland candidate is already present in the master.")
if(anyDuplicated(candidates$FixtureKey)) stop("Scotland recovery contains duplicate fixture keys.")
pair_key <- function(date,home,away) paste(date,norm(home),norm(away),sep="\r")
candidate_pairs <- pair_key(candidates$Date,candidates$Home,candidates$Away)
master_pairs <- pair_key(master_scotland$Date,master_scotland$CanonicalHome,master_scotland$CanonicalAway)
hits <- match(candidate_pairs,master_pairs)
if(any(!is.na(hits) & candidates$Score!=master_scotland$Score[hits]))
  stop("A Scotland candidate conflicts with an existing same-day score.")
master_team_dates <- unique(c(paste(master_scotland$Date,norm(master_scotland$CanonicalHome),sep="\r"),
                              paste(master_scotland$Date,norm(master_scotland$CanonicalAway),sep="\r")))
candidate_team_dates <- c(paste(candidates$Date,norm(candidates$Home),sep="\r"),
                          paste(candidates$Date,norm(candidates$Away),sep="\r"))
if(any(candidate_team_dates %chin% master_team_dates))
  stop("A Scotland candidate would double-book a club on an existing match date.")

new_rows <- data.table(Season=candidates$Season,Country="Scotland",Competition="scottish_premiership",
  CompetitionType="league",Tier=1L,League="Scottish Premiership",Date=candidates$Date,
  Home=candidates$Home,Away=candidates$Away,Result=candidates$Result,Score=candidates$Score,
  Source="rsssf",SourcePage=candidates$SourcePage,Stage="Recovered RSSSF Premiership fixture",
  DateApprox=FALSE,SourceFile=candidates$SourceFile,HomeAssociation="Scotland",AwayAssociation="Scotland")
for(column in setdiff(master_columns,names(new_rows))) new_rows[,(column):=NA]
new_rows <- new_rows[,..master_columns]
summary <- new_rows[,.(AddedMatches=.N,FirstDate=min(as.IDate(Date)),LastDate=max(as.IDate(Date))),by=Season][order(Season)]
print(summary)
message("Validated Scottish top-flight additions: ",nrow(new_rows),".")
if(!identical(Sys.getenv("APPLY_SCOTLAND_TOP_FLIGHT_RECOVERY"),"1")) {
  message("Preview only. Production master and alias file unchanged.")
} else {
  result <- rbindlist(list(master,new_rows),use.names=TRUE,fill=FALSE)
  setorder(result,Date,Country,Tier,Home,Away)
  exact <- result[,paste(Season,Country,Date,Home,Away,Score,sep="\r")]
  if(anyDuplicated(exact)) stop("Proposed master contains an exact duplicate fixture.")
  stamp <- format(Sys.time(),"%Y%m%d_%H%M%S")
  master_backup <- sub("[.]csv$",paste0("_before_scotland_top_flight_recovery_",stamp,".csv"),master_path)
  alias_backup <- sub("[.]csv$",paste0("_before_scotland_top_flight_recovery_",stamp,".csv"),alias_path)
  if(!file.copy(master_path,master_backup,overwrite=FALSE) || !file.copy(alias_path,alias_backup,overwrite=FALSE))
    stop("Could not create Scotland recovery backups.")
  fwrite(result,master_path,na="")
  fwrite(all_aliases,alias_path,na="")
  fwrite(summary,file.path(review,paste0("applied_import_",stamp,".csv")),na="")
  message("Production master updated: ",nrow(master)," -> ",nrow(result)," rows.")
  message("Master backup: ",master_backup)
  message("Alias backup: ",alias_backup)
}
