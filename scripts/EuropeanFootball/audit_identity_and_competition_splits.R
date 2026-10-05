# Review only: never modifies matches, aliases, ratings, or site JSON.
run_identity_split_audit <- function() {
  suppressPackageStartupMessages(library(data.table))
  stopifnot(requireNamespace("stringi", quietly=TRUE), requireNamespace("jsonlite", quietly=TRUE))
  started <- Sys.time()
  on.exit({cat("Elapsed:", round(as.numeric(difftime(Sys.time(), started, units="secs"))), "seconds\n")
    if(interactive() && requireNamespace("beepr", quietly=TRUE)) try(beepr::beep(), silent=TRUE)}, add=TRUE)
  root <- normalizePath(Sys.getenv("J_RATINGS_REPO", getwd()), winslash="/", mustWork=TRUE)
  base <- file.path(root, "EuropeanFootball/pipeline_data")
  out <- file.path(base, "Manual_Sources/Team_Identity_Audit/systematic_split_review")
  dir.create(out, recursive=TRUE, showWarnings=FALSE)
  norm <- function(x) {
    x <- tolower(stringi::stri_trans_general(x, "Latin-ASCII"))
    x <- gsub("[^a-z0-9 ]", " ", x)
    x <- gsub("\\b(fc|cf|fk|ac|sk|nk|afc)\\b", " ", x, perl=TRUE)
    trimws(gsub(" +", " ", x))
  }
  cat("[1/5] Reading master, aliases and published identities...\n")
  m <- fread(file.path(base,"Matches_Clean_Combined/european_football_all_matches.csv"), encoding="UTF-8")
  a <- fread(file.path(base,"Reference/team_aliases.csv"), encoding="UTF-8")
  a <- a[, if(uniqueN(CanonicalName)==1L) .(CanonicalName=CanonicalName[1]) else NULL, by=.(Country,SourceName)]
  amap <- setNames(a$CanonicalName, paste(tolower(a$Country),a$SourceName,sep="\r"))
  canon <- function(country,name) {
    for(i in 1:10) { y <- unname(amap[paste(tolower(country),name,sep="\r")]); use <- !is.na(y)&y!=name
      if(!any(use)) break
      name[use] <- y[use]
    }; name
  }
  m[, Date:=as.IDate(Date)]
  endpoint <- function(home) {
    name <- if(home) m$Home else m$Away
    opp <- if(home) m$Away else m$Home
    assoc <- if(home) m$HomeAssociation else m$AwayAssociation
    # Mirror 02's present keys so cross-border mistakes remain visible.
    country <- m$Country
    continental <- m$CompetitionType=="continental"
    country[continental] <- assoc[continental]
    data.table(Country=country, Name=canon(country,name), RawName=name,
      Association=assoc, Date=m$Date, Opponent=norm(opp), Side=if(home) "H" else "A",
      Score=m$Score, Competition=m$Competition, Type=m$CompetitionType,
      Season=m$Season, SourceFile=m$SourceFile)
  }
  p <- rbindlist(list(endpoint(TRUE),endpoint(FALSE)))
  p <- p[!is.na(Country)&nzchar(Country)&!is.na(Name)&nzchar(Name)&!is.na(Date)]
  p[, CountryKey:=tolower(trimws(Country))]
  p[, Identity:=paste(CountryKey,Name,sep="|")]
  inventory <- p[,.(Country=Country[1],Name=Name[1],Appearances=.N,
    FirstDate=min(Date),LastDate=max(Date),Seasons=uniqueN(Season),
    Competitions=paste(sort(unique(Competition)),collapse="; "),
    RawNames=paste(sort(unique(RawName)),collapse="; "),
    LeagueAppearances=sum(Type=="league",na.rm=TRUE),
    ContinentalAppearances=sum(Type=="continental",na.rm=TRUE)),by=.(Identity,CountryKey)]
  inventory[, `:=`(NameKey=norm(Name), TokenKey=vapply(strsplit(norm(Name)," "),function(x) paste(sort(x),collapse=" "),character(1)))]
  site <- as.data.table(jsonlite::fromJSON(file.path(root,"EuropeanFootball/data/teams.json")))
  site[,Identity:=paste(tolower(country),name,sep="|")]
  inventory[, Published:=Identity %chin% site$Identity]
  fwrite(inventory,file.path(out,"club_inventory.csv"),na="")
  cat("[2/5] Comparing names within countries and exact names across countries...\n")
  pairs <- list(); k <- 0L
  add <- function(i,j,reason) {
    k <<- k+1L
    pairs[[k]] <<- data.table(A=inventory$Identity[i],B=inventory$Identity[j],Reason=reason)
  }
  countries <- unique(inventory$CountryKey)
  for(ci in seq_along(countries)) {
    ids <- which(inventory$CountryKey==countries[ci]); keys <- inventory$NameKey[ids]
    if(length(ids)>1L) {
      distance <- adist(keys)
      for(ii in seq_len(length(ids)-1L)) for(jj in (ii+1L):length(ids)) {
        x<-keys[ii]; y<-keys[jj]; minlen<-min(nchar(x),nchar(y))
        exact<-nzchar(x)&&inventory$TokenKey[ids[ii]]==inventory$TokenKey[ids[jj]]
        short<-minlen>=4L && (startsWith(x,paste0(y," "))||startsWith(y,paste0(x," ")))
        fuzzy<-minlen>=5L && distance[ii,jj]/max(nchar(x),nchar(y))<=0.22
        if(exact||short||fuzzy) add(ids[ii],ids[jj],if(exact) "NORMALISED_OR_REORDERED_NAME" else if(short) "SHORTENED_NAME" else "SIMILAR_NAME")
      }
    }
    if(ci%%10L==0L||ci==length(countries)) cat("  Countries checked:",ci,"/",length(countries),"\n")
  }
  groups <- split(seq_len(nrow(inventory)),inventory$TokenKey)
  for(ids in groups) if(length(ids)>1L && nzchar(inventory$TokenKey[ids[1]])) {
    z<-combn(ids,2)
    for(j in seq_len(ncol(z))) if(inventory$CountryKey[z[1,j]]!=inventory$CountryKey[z[2,j]])
      add(z[1,j],z[2,j],"CROSS_COUNTRY_NAME_REVIEW")
  }
  candidates <- if(length(pairs)) unique(rbindlist(pairs)) else data.table(A=character(),B=character(),Reason=character())
  cat("[3/5] Finding shared date, side, score and opponent evidence...\n")
  evidence <- unique(p[!is.na(Score)&nzchar(Score),.(Date,Side,Score,Opponent,Identity)])
  evidence[, SignatureCount:=.N, by=.(Date,Side,Score,Opponent)]
  # Exclude crowded signatures: generic opponent labels can otherwise explode.
  e <- evidence[SignatureCount>1L & SignatureCount<=6L]
  votes <- merge(e,e,by=c("Date","Side","Score","Opponent"),allow.cartesian=TRUE)
  votes <- votes[Identity.x<Identity.y]
  support <- votes[,.(MatchingFixtureDates=uniqueN(Date),ExampleDates=paste(head(sort(unique(Date)),5),collapse="; ")),
    by=.(A=Identity.x,B=Identity.y)]
  # Fixture evidence may discover pairs with completely different names.
  extra <- support[MatchingFixtureDates>=2L,.(A,B,Reason="REPEATED_FIXTURE_EVIDENCE")]
  candidates[, `:=`(AA=pmin(A,B),BB=pmax(A,B))]
  candidates[, `:=`(A=AA,B=BB)]; candidates[,c("AA","BB"):=NULL]
  candidates <- unique(rbindlist(list(candidates,extra)))
  candidates <- candidates[,.(Reasons=paste(sort(unique(Reason)),collapse="; ")),by=.(A,B)]
  candidates <- merge(candidates,support,by=c("A","B"),all.x=TRUE)
  candidates[is.na(MatchingFixtureDates),MatchingFixtureDates:=0L]
  left <- copy(inventory); right <- copy(inventory)
  setnames(left,names(left),paste0(names(left),"A"));setnames(left,"IdentityA","A")
  setnames(right,names(right),paste0(names(right),"B"));setnames(right,"IdentityB","B")
  candidates <- merge(merge(candidates,left,by="A"),right,by="B")
  candidates[, HistoryOverlap:=FirstDateA<=LastDateB & FirstDateB<=LastDateA]
  candidates[, Priority:=fifelse(MatchingFixtureDates>=2,"1_FIXTURE_EVIDENCE",
    fifelse(PublishedA & PublishedB & CountryKeyA==CountryKeyB,"2_TWO_PUBLISHED_IDENTITIES",
    fifelse(PublishedA & PublishedB,"3_CROSS_BORDER_REVIEW","4_HISTORICAL_NAME_REVIEW")))]
  setorder(candidates,Priority,-MatchingFixtureDates,-AppearancesA)
  fwrite(candidates,file.path(out,"identity_pairs_review.csv"),na="")
  cat("[4/5] Checking association disagreements and competition labels...\n")
  disagreements <- p[!is.na(Association)&nzchar(Association)&tolower(Association)!=CountryKey,
    .(Appearances=.N,FirstDate=min(Date),LastDate=max(Date),ExampleSource=SourceFile[1]),
    by=.(Country,Name,Association,Competition,Type)]
  fwrite(disagreements,file.path(out,"association_disagreements.csv"),na="")
  membership <- p[Type=="league",.(Appearances=.N,FirstDate=min(Date),LastDate=max(Date),
    ExampleSource=SourceFile[1]),by=.(Country,Name,Competition,Season)]
  membership[, DifferentLeagues:=uniqueN(Competition),by=.(Country,Name,Season)]
  fwrite(membership[DifferentLeagues>1L],file.path(out,"multiple_leagues_same_season.csv"),na="")
  # Labels alone cannot prove membership; expose all source-backed assignments.
  fwrite(membership,file.path(out,"league_assignments.csv"),na="")
  canada <- p[CountryKey %chin% c("canada","united states") &
    (grepl("toronto|vancouver|montreal|edmonton|scrosoppi|laval|rovers",norm(Name)) | CountryKey=="canada"),
    .(Appearances=.N,FirstDate=min(Date),LastDate=max(Date)),by=.(Country,Name,Association,Competition,Type,SourceFile)]
  fwrite(canada,file.path(out,"canada_cross_border_review.csv"),na="")
  fwrite(site[country!=tools::toTitleCase(tolower(country))],file.path(out,"country_label_review.csv"),na="")
  cat("[5/5] Review summary:\n")
  print(candidates[,.(Pairs=.N),by=Priority])
  cat("Association disagreements:",nrow(disagreements),"\nOutput:",out,"\n")
  writeLines(c("Review candidates only. No automatic merges or production changes.",
    "Shared fixtures can also be duplicated source rows, not proof of club continuity.",
    "Cross-country shared names often belong to unrelated clubs; review source evidence.",
    "Similar names can be reserves, women, successor clubs or separate clubs.",
    "Multiple leagues can reflect promotion or stage naming, not an error.",
    "Missing association fields cannot be used to disprove cross-border mistakes.",
    "This audit finds candidates, not a completeness guarantee. Unrelated spelling changes may escape it."),file.path(out,"READ_ME.txt"))
}
run_identity_split_audit()
