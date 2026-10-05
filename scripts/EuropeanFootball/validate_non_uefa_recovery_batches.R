# Validate substantial cached non-UEFA recovery batches and prepare an import
# preview. Reads saved extraction + local standings; never writes production.
run_non_uefa_batch_validation <- function() {
  suppressPackageStartupMessages(library(data.table))
  source("scripts/EuropeanFootball/rsssf_roster_abbreviations.R")
  on.exit(if(interactive() && requireNamespace("beepr",quietly=TRUE)) try(beepr::beep(),silent=TRUE),add=TRUE)
  root<-normalizePath(getwd(),winslash="/",mustWork=TRUE)
  base<-file.path(root,"EuropeanFootball/pipeline_data")
  saved<-file.path(base,"Manual_Sources/RSSSF_World_Refresh_Review")
  out<-file.path(saved,Sys.getenv("NON_UEFA_VALIDATION_OUTPUT","batch_validation"))
  dir.create(out,recursive=TRUE,showWarnings=FALSE)
  cat("[1/4] Reading substantial recovery seasons and current master...\n")
  summary<-fread(file.path(saved,"current_situation/domestic_season_summary.csv"))
  targets<-summary[PotentialMissing>=20L]
  selected<-trimws(strsplit(Sys.getenv("NON_UEFA_VALIDATION_COUNTRIES",""),";",fixed=TRUE)[[1]])
  selected<-selected[nzchar(selected)]
  if(length(selected)) targets<-targets[Country %chin% selected]
  selected_seasons<-strsplit(Sys.getenv("NON_UEFA_VALIDATION_SEASONS",""),";",fixed=TRUE)[[1]]
  selected_seasons<-selected_seasons[nzchar(selected_seasons)]
  if(length(selected_seasons)) targets<-targets[paste(Country,Season,sep="|") %chin% selected_seasons]
  if(identical(Sys.getenv("NON_UEFA_FAILED_STANDINGS_ONLY"),"1")) {
    prior<-fread(file.path(saved,"new_roster_club_recheck/batch_summary.csv"))
    failed<-prior[is.na(StandingsTeams),.(Country,Season)]
    targets<-merge(targets,failed,by=c("Country","Season"))
  }
  if(!nrow(targets)) stop("No substantial batches to validate.")
  g<-fread(file.path(saved,"all_dated_rsssf_games.csv"))
  cmap<-c("Hongkong"="Hong Kong","Macao"="Macau","East Timor"="Timor-Leste",
    "Congo-Brazzaville"="Congo","Congo-Kinshasa"="DR Congo","Guinea Bissau"="Guinea-Bissau",
    "French Guyana"="French Guiana","US Virgin Islands"="United States Virgin Islands",
    "Surinam"="Suriname","Fiji (clubs)"="Fiji","Fiji (districts)"="Fiji","Fiji (national)"="Fiji",
    "Vanuatu (PVFL)"="Vanuatu","Vanuatu (VFFCL)"="Vanuatu")
  country<-function(x) { h<-unname(cmap[x]); x[!is.na(h)]<-h[!is.na(h)]; x }
  g[,Country:=country(Country)]
  g<-merge(g,targets[,.(Country,Season)],by=c("Country","Season"))
  g<-g[!(tolower(as.character(Annotated)) %chin% c("true","t","1"))]
  g[,`:=`(Date=as.IDate(Date),StartYear=suppressWarnings(as.integer(StartYear)))]
  m<-fread(file.path(base,"Matches_Clean_Combined/european_football_all_matches.csv"),showProgress=FALSE)
  m[,`:=`(Country=country(Country),Date=as.IDate(Date))]
  dom<-m[CompetitionType=="league" & Tier==1L]
  aliases<-fread(file.path(base,"Reference/team_aliases.csv"))
  aliases[,Country:=country(Country)]
  aliases<-aliases[,if(uniqueN(CanonicalName)==1L) .(CanonicalName=CanonicalName[1]) else NULL,by=.(Country,SourceName)]
  amap<-setNames(aliases$CanonicalName,paste(aliases$Country,aliases$SourceName,sep="\r"))
  canonical<-function(c,x) {
    for(i in 1:10) { h<-unname(amap[paste(c,x,sep="\r")]); use<-!is.na(h)&h!=x
      if(!any(use)) break; x[use]<-h[use] }; x
  }
  norm<-function(x) gsub("[^a-z0-9]","",tolower(stringi::stri_trans_general(x,"Latin-ASCII")))
  g[,`:=`(Home=canonical(Country,Home),Away=canonical(Country,Away))]
  dom[,`:=`(Home=canonical(Country,Home),Away=canonical(Country,Away))]
  g[,`:=`(H=norm(Home),A=norm(Away))]
  dom[,`:=`(H=norm(Home),A=norm(Away))]
  key<-function(d) paste(d$Country,d$Date,d$H,d$A,d$Score,sep="\r")
  g[,Present:=key(g) %chin% key(dom)]
  g[,Duplicate:=duplicated(key(g))]
  known<-unique(rbind(dom[,.(Country,Name=H)],dom[,.(Country,Name=A)],
    aliases[,.(Country,Name=norm(CanonicalName))]))
  nk<-paste(known$Country,known$Name,sep="\r")
  g[,UnknownIdentity:=!paste(Country,H,sep="\r") %chin% nk | !paste(Country,A,sep="\r") %chin% nk]
  # Opt-in recovery of clubs missing from the master. Standings and repeated
  # appearances establish source membership; close existing names stay held.
  allow_roster_new<-identical(Sys.getenv("NON_UEFA_ALLOW_ROSTER_NEW_CLUBS"),"1")
  independent_keys<-character()
  if(allow_roster_new) {
    cat("  Checking unknown clubs against existing names before roster admission...\n")
    endpoints<-rbind(g[,.(Country,Label=Home,Name=H,Date)],g[,.(Country,Label=Away,Name=A,Date)])
    endpoints<-endpoints[!paste(Country,Name,sep="\r") %chin% nk]
    unknown_clubs<-endpoints[,.(Label=Label[1],Dates=uniqueN(Date)),by=.(Country,Name)]
    known_labels<-unique(rbind(dom[,.(Country,Label=Home)],dom[,.(Country,Label=Away)],
      aliases[,.(Country,Label=canonical(Country,CanonicalName))]))
    core<-function(x) norm(gsub("\\b(FC|SC|CF|FK|AFC)\\b","",x,ignore.case=TRUE,perl=TRUE))
    decisions<-vector("list",nrow(unknown_clubs))
    for(j in seq_len(nrow(unknown_clubs))) {
      u<-unknown_clubs[j]; labels<-known_labels[Country==u$Country,Label]
      keys<-norm(labels); uc<-core(u$Label); kc<-core(labels)
      distance<-as.integer(adist(u$Name,keys))
      similar<-length(keys)>0L && any(1-distance/pmax(nchar(u$Name),nchar(keys))>=0.85)
      contains<-length(kc)>0L && any(vapply(kc,function(k) nchar(k)>=5L && nchar(uc)>=5L &&
        (grepl(k,uc,fixed=TRUE)||grepl(uc,k,fixed=TRUE)),logical(1)))
      expanded<-length(rsssf_roster_abbreviation_match(u$Label,labels))>0L
      ambiguous<-similar||contains||expanded
      decisions[[j]]<-data.table(Country=u$Country,SourceName=u$Label,Key=u$Name,
        DistinctDates=u$Dates,PossibleExistingAlias=ambiguous,
        RosterAdmissionEligible=u$Dates>=4L && !ambiguous)
      if(j==1L || j%%25L==0L || j==nrow(unknown_clubs))
        cat("    Club ",j,"/",nrow(unknown_clubs),"\n",sep="")
    }
    admission<-if(length(decisions)) rbindlist(decisions) else
      data.table(Country=character(),SourceName=character(),Key=character(),DistinctDates=integer(),
        PossibleExistingAlias=logical(),RosterAdmissionEligible=logical())
    fwrite(admission,file.path(out,"new_roster_club_evidence.csv"),na="")
    independent_keys<-paste(admission[RosterAdmissionEligible==TRUE,Country],
      admission[RosterAdmissionEligible==TRUE,Key],sep="\r")
  }
  schedules<-unique(c(paste(dom$Country,dom$Date,dom$H,sep="\r"),paste(dom$Country,dom$Date,dom$A,sep="\r")))
  g[,ClubDateConflict:=!Present & (paste(Country,Date,H,sep="\r") %chin% schedules |
                                                paste(Country,Date,A,sep="\r") %chin% schedules)]
  pair<-function(d) paste(d$Country,d$H,d$A,sep="\r")
  dates<-split(as.integer(dom$Date),pair(dom)); gp<-pair(g)
  g[,NearbyFixture:=vapply(seq_len(.N),function(i) {
    z<-dates[[gp[i]]]; length(z)>0L && any(abs(as.integer(Date[i])-z)<=1L)
  },logical(1))]
  g[,NumberedRound:=grepl("^Round [0-9]+(?:$|[[:space:]]|\\[)",RSSSFStage,perl=TRUE)]
  g[,CupBlock:=grepl("cup|copa|kubok|karikas|kupa|coupe|pokal|beker",RSSSFSection,ignore.case=TRUE)]
  g[,BadDate:=is.na(Date) | is.na(StartYear) | as.integer(format(Date,"%Y"))<StartYear |
       as.integer(format(Date,"%Y"))>StartYear+1L]
  g[,BadScore:=!grepl("^[0-9]+-[0-9]+$",Score) | H==A]
  # Result encoding must agree with the saved score.
  sp<-tstrsplit(g$Score,"-",fixed=TRUE)
  expected_result<-fifelse(as.integer(sp[[1]])>as.integer(sp[[2]]),"1-0",
    fifelse(as.integer(sp[[1]])<as.integer(sp[[2]]),"0-1","0.5-0.5"))
  g[,BadScore:=BadScore | is.na(expected_result) | Result!=expected_result]
  cat("[2/4] Checking local standings and regular-stage fixture structure...\n")
  standings<-function(path) {
    tryCatch({
      doc<-xml2::read_html(path)
      pre<-xml2::xml_find_first(doc,"//h4[1]/following::pre[1]")
      if(inherits(pre,"xml_missing")) pre<-xml2::xml_find_first(doc,"//pre[1]")
      lines<-strsplit(xml2::xml_text(pre),"\n",fixed=TRUE)[[1]]
      boundary<-grep("Cross.Table|^Round [0-9]+|^\\[[A-Za-z]+ [0-9]",trimws(lines),ignore.case=TRUE)
      if(length(boundary)) lines<-head(lines,min(boundary)-1L)
      # Require the goals column after P/W/D/L. Otherwise a number in a club
      # name (e.g. Samartex 1996) can be mistaken for games played.
      # Unranked '-.' clubs with a numeric played record still contribute to
      # the season's fixtures, unless their results were explicitly annulled.
      rx<-"^\\s*(?:[0-9]+|-)[.]\\s*(.*?)\\s+([0-9]+)\\s+([0-9]+)\\s+([0-9]+)\\s+([0-9]+)\\s+[0-9]+\\s*[-:]\\s*[0-9]+"
      hits<-regmatches(lines,regexec(rx,lines,perl=TRUE))
      hits<-hits[lengths(hits)==6L]
      hits<-hits[!vapply(hits,function(z) grepl("annulled",z[1],ignore.case=TRUE),logical(1))]
      # Stop at the second ranking starting with 1. Overall, opening and closing
      # stage tables must never be added together as if they were separate clubs.
      ranks<-vapply(hits,function(z) suppressWarnings(as.integer(sub("^\\s*([0-9]+)[.].*$","\\1",z[1]))),integer(1))
      reset<-which(ranks==1L)
      if(length(reset)>1L) hits<-head(hits,reset[2L]-1L)
      if(length(hits)<4L) return(list(Teams=NA_integer_,Expected=NA_integer_,Roster=character()))
      p<-vapply(hits,function(z) as.integer(z[3]),integer(1))
      valid<-vapply(hits,function(z) as.integer(z[3])==sum(as.integer(z[4:6])),logical(1))
      if(!all(valid) || sum(p)%%2L!=0L) return(list(Teams=NA_integer_,Expected=NA_integer_,Roster=character()))
      list(Teams=length(hits),Expected=as.integer(sum(p)/2L),Roster=vapply(hits,`[`,character(1),2L))
    },error=function(e) list(Teams=NA_integer_,Expected=NA_integer_,Roster=character()))
  }
  reports<-vector("list",nrow(targets))
  name_checks<-list()
  g[,RosterPair:=FALSE]
  g[,`:=`(RosterH=NA_character_,RosterA=NA_character_)]
  for(i in seq_len(nrow(targets))) {
    ct<-targets$Country[i]; ss<-targets$Season[i]
    pg<-g[Country==ct & Season==ss]
    paths<-unique(pg$SourceFile[file.exists(pg$SourceFile)])
    st<-if(length(paths)==1L) standings(paths[1]) else list(Teams=NA_integer_,Expected=NA_integer_,Roster=character())
    roster_labels<-canonical(rep(ct,length(st$Roster)),st$Roster)
    roster_keys<-norm(roster_labels)
    roster_core<-function(x) {
      x<-gsub("\\([^)]*\\)","",x)
      x<-gsub("\\b(FC|SC|CF|FK|AFC)\\b","",x,ignore.case=TRUE,perl=TRUE)
      norm(x)
    }
    # Preserve literal standings names: approved aliases may replace an old
    # club name with its modern name, hiding the abbreviation used on this page.
    cores<-roster_core(roster_labels)
    raw_cores<-roster_core(st$Roster)
    initialism<-function(x) {
      x<-gsub("\\([^)]*\\)","",x)
      x<-gsub("\\b(FC|SC|CF|FK|AFC)\\b","",x,ignore.case=TRUE,perl=TRUE)
      words<-strsplit(trimws(x),"[[:space:]-]+",perl=TRUE)[[1]]
      if(length(words)<3L) return("")
      norm(paste(substr(words,1L,1L),collapse=""))
    }
    initials<-vapply(st$Roster,initialism,character(1))
    local_roster_match<-function(x,raw) {
      exact<-which(norm(x)==roster_keys | norm(raw)==norm(st$Roster))
      if(length(exact)==1L) return(roster_keys[exact])
      short<-roster_core(x)
      raw_short<-roster_core(raw)
      exact<-which(short==cores | raw_short==raw_cores)
      if(length(exact)==1L) return(roster_keys[exact])
      # Only literal uppercase abbreviations of at least three letters qualify.
      # An initialism proves local roster membership, not a global club alias.
      if(grepl("^[A-Z]{3,6}$",trimws(raw))) {
        hit<-which(nzchar(initials) & norm(raw)==initials)
        if(length(hit)==1L) return(roster_keys[hit])
      }
      expanded<-rsssf_roster_abbreviation_match(raw,st$Roster)
      if(length(expanded)==1L) return(roster_keys[expanded])
      contains<-function(a,b) nchar(a)>=5L && nchar(b)>=5L &&
        (grepl(a,b,fixed=TRUE)||grepl(b,a,fixed=TRUE))
      hits<-which(vapply(seq_along(cores),function(j)
        contains(short,cores[j]) || contains(raw_short,raw_cores[j]),logical(1)))
      if(length(hits)==1L) roster_keys[hits] else NA_character_
    }
    # An exact approved canonical roster match proves membership, even when the
    # parser accidentally labelled opening/closing league stages championship.
    # Single-character variants are proposed for review, not imported silently.
    observed<-unique(c(pg$Home,pg$Away))
    unknown<-observed[!norm(observed) %chin% roster_keys]
    for(nm in unknown) {
      distances<-as.integer(adist(norm(nm),roster_keys))
      hit<-which(distances==1L & nchar(norm(nm))>=8L)
      if(length(hit)==1L) name_checks[[length(name_checks)+1L]]<-data.table(
        Country=ct,Season=ss,SourceName=nm,SuggestedRosterName=roster_labels[hit],Method="unique_one_character_roster_variant")
    }
    # Resolve each spelling once per season, rather than once per appearance.
    spellings<-unique(rbind(pg[,.(Canonical=Home,Raw=RHome)],pg[,.(Canonical=Away,Raw=RAway)]))
    members<-vapply(seq_len(nrow(spellings)),function(j)
      local_roster_match(spellings$Canonical[j],spellings$Raw[j]),character(1))
    member_map<-setNames(members,paste(spellings$Canonical,spellings$Raw,sep="\r"))
    home_members<-unname(member_map[paste(pg$Home,pg$RHome,sep="\r")])
    away_members<-unname(member_map[paste(pg$Away,pg$RAway,sep="\r")])
    g[Country==ct & Season==ss,`:=`(RosterH=home_members,RosterA=away_members,
      RosterPair=!is.na(home_members) & !is.na(away_members) & home_members!=away_members)]
    pg<-g[Country==ct & Season==ss]
    regular<-pg[NumberedRound==TRUE & CupBlock==FALSE & Duplicate==FALSE & RosterPair==TRUE]
    appearances<-rbind(regular[,.(Name=RosterH,Date)],regular[,.(Name=RosterA,Date)])
    clubs<-uniqueN(appearances$Name)
    internal_clashes<-anyDuplicated(appearances)>0L
    # Partial extraction is useful. Require coherent roster membership and a
    # plausible count, rather than rejecting every row when one game is absent.
    structure_ok<-!is.na(st$Expected) && st$Teams==clubs &&
      nrow(regular)>=0.90*st$Expected && nrow(regular)<=st$Expected && !internal_clashes
    reports[[i]]<-data.table(Country=ct,Season=ss,StandingsTeams=st$Teams,
      StandingsExpectedRegularGames=st$Expected,ParsedRegularGames=nrow(regular),
      ObservedRegularTeams=clubs,SameDayClash=internal_clashes,StructurePass=structure_ok,
      PartialSeason=structure_ok && nrow(regular)<st$Expected,
      SourceFile=if(length(paths)==1L) paths[1] else paste(paths,collapse=" | "))
    if(i==1L || i%%10L==0L || i==nrow(targets)) cat("  Checked ",i,"/",nrow(targets),": ",ct," ",ss,"\n",sep="")
  }
  page<-rbindlist(reports)
  g<-merge(g,page[,.(Country,Season,StructurePass)],by=c("Country","Season"))
  g[,RosterIdentitySupported:=allow_roster_new & StructurePass & RosterPair &
    paste(Country,H,sep="\r") %chin% c(nk,independent_keys) &
    paste(Country,A,sep="\r") %chin% c(nk,independent_keys)]
  g[,Decision:=fcase(Present,"ALREADY_PRESENT",Duplicate,"DUPLICATE_REVIEW",BadDate|BadScore,"INVALID_DATE_OR_SCORE",
    CupBlock,"CUP_BOUNDARY_REVIEW",!NumberedRound,"PLAYOFF_OR_OTHER_STAGE_REVIEW",
    !RosterPair,"ROSTER_NAME_OR_OTHER_DIVISION_REVIEW",
    NearbyFixture,"NEARBY_FIXTURE_REVIEW",ClubDateConflict,"CLUB_DATE_CONFLICT_REVIEW",
    UnknownIdentity & !RosterIdentitySupported,"IDENTITY_REVIEW",!StructurePass,"SEASON_STRUCTURE_REVIEW",default="VALIDATED_IMPORT_PREVIEW")]
  metadata<-dom[, .SD[which.max(as.integer(Date))],by=Country,.SDcols=c("Competition","League")]
  g[!Country %chin% metadata$Country & Decision=="VALIDATED_IMPORT_PREVIEW",Decision:="NO_LEAGUE_METADATA_REVIEW"]
  cat("[3/4] Writing import preview and held rows...\n")
  add<-g[Decision=="VALIDATED_IMPORT_PREVIEW"]
  for(ct in unique(add$Country)) {
    meta<-metadata[Country==ct]
    add[Country==ct,`:=`(Competition=meta$Competition[1],League=meta$League[1])]
  }
  if(nrow(add)) add[,`:=`(Tier=1L,CompetitionType="league",Source="rsssf",DateApprox=FALSE,
                          HomeAssociation=Country,AwayAssociation=Country)]
  fwrite(add,file.path(out,"validated_import_preview.csv"),na="")
  fwrite(g[!Decision %chin% c("ALREADY_PRESENT","VALIDATED_IMPORT_PREVIEW")],file.path(out,"held_fixture_review.csv"),na="")
  batches<-g[,.(ParsedDated=.N,AlreadyPresent=sum(Present),ValidatedAdditions=sum(Decision=="VALIDATED_IMPORT_PREVIEW"),
    IdentityReview=sum(Decision=="IDENTITY_REVIEW"),OtherHeld=sum(!Decision %chin% c("ALREADY_PRESENT","VALIDATED_IMPORT_PREVIEW","IDENTITY_REVIEW"))),
    by=.(Country,Season)]
  batches<-merge(batches,page,by=c("Country","Season"))
  setorder(batches,-ValidatedAdditions,Country,Season)
  fwrite(batches,file.path(out,"batch_summary.csv"),na="")
  fwrite(if(length(name_checks)) rbindlist(name_checks) else
    data.table(Country=character(),Season=character(),SourceName=character(),SuggestedRosterName=character(),Method=character()),
    file.path(out,"roster_name_suggestions.csv"),na="")
  fwrite(g[,.(Rows=.N),by=Decision],file.path(out,"decision_summary.csv"))
  cat("[4/4] Validation overview:\n")
  print(g[,.(Rows=.N),by=Decision][order(-Rows)])
  print(batches[ValidatedAdditions>0L,.(Country,Season,ValidatedAdditions,StandingsExpectedRegularGames,ParsedRegularGames)])
  cat("Validated preview additions: ",nrow(add),". These have not been imported.\n",sep="")
  cat("Whole-season standings checks are conservative; held batches are not declared unrecoverable.\n")
  cat("No production changes or web requests. Report: ",out,"\n",sep="")
}
run_non_uefa_batch_validation()
