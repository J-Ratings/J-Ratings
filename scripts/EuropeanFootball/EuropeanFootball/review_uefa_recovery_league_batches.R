# Review already parsed candidates; no parsing, downloads or production writes.
suppressPackageStartupMessages(library(data.table))
root <- normalizePath(getwd(),winslash="/",mustWork=TRUE)
out <- file.path(root,"EuropeanFootball/pipeline_data/Manual_Sources/UEFA/cached_page_recovery")
g <- fread(file.path(out,"parser/all_dated_rsssf_games.csv"))
expected <- data.table(Country=c("Belarus","Faroe Islands","Georgia","Iceland","Kazakhstan","Sweden","Ukraine"),
  Season=c(rep("2025",6),"2022/23"),ExpectedLeagueMatches=c(240L,135L,180L,162L,182L,240L,240L))
g <- merge(g,expected,by=c("Country","Season"))
# All seven sources label scheduled league rounds with a numbered Round heading.
# Inter-division playoffs are separate First/Second Leg blocks.
g[, LeagueRound := grepl("^Round [0-9]+(?:$|[[:space:]]|\\[)",RSSSFStage,perl=TRUE)]
g[, AnnotatedRow := tolower(as.character(Annotated)) %chin% c("true","t","1")]
g[, FixtureKey := paste(Country,Date,Home,Away,Score,sep="\r")]
summary <- g[,.(ParsedDatedRows=.N,NonLeagueRoundRows=sum(!LeagueRound),
  AnnotatedRows=sum(AnnotatedRow),LeagueMatches=sum(LeagueRound & !AnnotatedRow),
  DuplicateLeagueRows=sum(duplicated(FixtureKey[LeagueRound & !AnnotatedRow])),
  Clubs=uniqueN(c(Home[LeagueRound & !AnnotatedRow],Away[LeagueRound & !AnnotatedRow])),
  FirstDate=min(Date[LeagueRound & !AnnotatedRow]),LastDate=max(Date[LeagueRound & !AnnotatedRow])),
  by=.(Country,Season,ExpectedLeagueMatches)]
summary[, CountCheck:=fifelse(LeagueMatches==ExpectedLeagueMatches & DuplicateLeagueRows==0L,"PASS","REVIEW")]
fwrite(summary,file.path(out,"verified_batch_summary.csv"))
fwrite(g[LeagueRound & !AnnotatedRow],file.path(out,"reviewed_league_candidates.csv"))
fwrite(g[!LeagueRound | AnnotatedRow],file.path(out,"excluded_batch_rows.csv"))
print(summary)
cat("Candidates remain review files. Production master unchanged.\n")
if(interactive() && requireNamespace("beepr",quietly=TRUE)) beepr::beep()
