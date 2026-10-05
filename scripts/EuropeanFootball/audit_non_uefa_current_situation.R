# Diagnostic comparison of saved non-UEFA extraction with CURRENT master.
# No parsing, downloads, imports, alias changes or Elo calculations.
run_non_uefa_situation <- function() {
  suppressPackageStartupMessages(library(data.table))
  on.exit(if(interactive() && requireNamespace("beepr",quietly=TRUE)) try(beepr::beep(),silent=TRUE),add=TRUE)
  root <- normalizePath(getwd(),winslash="/",mustWork=TRUE)
  base <- file.path(root,"EuropeanFootball/pipeline_data")
  saved <- file.path(base,"Manual_Sources",Sys.getenv("NON_UEFA_SITUATION_INPUT_FOLDER","RSSSF_World_Audit"))
  out <- file.path(saved,"current_situation")
  dir.create(out,recursive=TRUE,showWarnings=FALSE)
  cat("[1/5] Reading saved domestic audit and current master...\n")
  aliases <- fread(file.path(base,"Reference/team_aliases.csv"))
  seeds <- unique(fread(file.path(base,"Reference/non_uefa_country_seeds.csv"))[
    Tier==1L & Confederation!="UEFA",.(Country,Confederation)])
  country_alias <- c("Hongkong"="Hong Kong","Macao"="Macau","East Timor"="Timor-Leste",
    "Congo-Brazzaville"="Congo","Congo-Kinshasa"="DR Congo","Guinea Bissau"="Guinea-Bissau",
    "French Guyana"="French Guiana","US Virgin Islands"="United States Virgin Islands",
    "Surinam"="Suriname","Fiji (clubs)"="Fiji","Fiji (districts)"="Fiji",
    "Fiji (national)"="Fiji","Vanuatu (PVFL)"="Vanuatu","Vanuatu (VFFCL)"="Vanuatu")
  country <- function(x) { h<-unname(country_alias[x]); x[!is.na(h)]<-h[!is.na(h)]; x }
  aliases[,Country:=country(Country)]
  aliases <- aliases[,if(uniqueN(CanonicalName)==1L) .(CanonicalName=CanonicalName[1]) else NULL,by=.(Country,SourceName)]
  amap <- setNames(aliases$CanonicalName,paste(aliases$Country,aliases$SourceName,sep="\r"))
  canon <- function(c,x) {
    for(i in 1:10) { h<-unname(amap[paste(c,x,sep="\r")]); use<-!is.na(h)&h!=x
      if(!any(use)) break; x[use]<-h[use] }; x
  }
  norm <- function(x) gsub("[^a-z0-9]","",tolower(stringi::stri_trans_general(x,"Latin-ASCII")))
  m <- fread(file.path(base,"Matches_Clean_Combined/european_football_all_matches.csv"),showProgress=FALSE)
  m[,`:=`(Country=country(Country),Date=as.IDate(Date))]
  d <- m[CompetitionType=="league" & Tier==1L & Country %in% seeds$Country]
  d[,`:=`(H=norm(canon(Country,Home)),A=norm(canon(Country,Away)))]
  g <- fread(file.path(saved,"all_dated_rsssf_games.csv"))
  g[,`:=`(Country=country(Country),Date=as.IDate(Date))]
  g <- g[Country %in% seeds$Country & CompetitionType=="league" & StartYear>=2010L & StartYear<=2025L &
    !is.na(Date) & !(tolower(as.character(Annotated)) %chin% c("true","t","1"))]
  g[,`:=`(H=norm(canon(Country,Home)),A=norm(canon(Country,Away)))]
  key <- function(z) paste(z$Country,z$Date,z$H,z$A,z$Score,sep="\r")
  g[,Existing:=key(g) %chin% key(d)]
  cat("[2/5] Testing repeated naming evidence within each association...\n")
  signature <- function(z) {
    q<-unique(z[,.(Country,Date,Score,H,A)])
    q[,if(.N==1L) .SD else NULL,by=.(Country,Date,Score)]
  }
  pairs <- merge(signature(g),signature(d),by=c("Country","Date","Score"),suffixes=c("Source","Target"))
  votes<-unique(rbind(pairs[,.(Country,Date,Source=HSource,Target=HTarget)],
                      pairs[,.(Country,Date,Source=ASource,Target=ATarget)]))
  votes<-votes[,.(Support=uniqueN(Date)),by=.(Country,Source,Target)]
  votes[,Total:=sum(Support),by=.(Country,Source)]
  setorder(votes,Country,Source,-Support)
  proposed<-votes[, .SD[1L],by=.(Country,Source)]
  proposed[,AcceptedForComparison:=Support>=8L & Support/Total>=0.95]
  known<-unique(rbind(d[,.(Country,Name=H)],d[,.(Country,Name=A)]))
  proposed[paste(Country,Source,sep="\r") %chin% paste(known$Country,known$Name,sep="\r") & Source!=Target,
    AcceptedForComparison:=FALSE]
  fwrite(proposed[Source!=Target],file.path(out,"domestic_name_evidence.csv"))
  inference<-proposed[AcceptedForComparison==TRUE]
  imap<-setNames(inference$Target,paste(inference$Country,inference$Source,sep="\r"))
  inferred<-function(c,x) { h<-unname(imap[paste(c,x,sep="\r")]); x[!is.na(h)]<-h[!is.na(h)]; x }
  g[,`:=`(H=inferred(Country,H),A=inferred(Country,A))]
  g[,AfterNames:=key(g) %chin% key(d)]
  bookings<-unique(c(paste(d$Country,d$Date,d$H,sep="\r"),paste(d$Country,d$Date,d$A,sep="\r")))
  g[,ClubDateConflict:=paste(Country,Date,H,sep="\r") %chin% bookings | paste(Country,Date,A,sep="\r") %chin% bookings]
  g[,NumberedRound:=grepl("^Round [0-9]+(?:$|[[:space:]]|\\[)",RSSSFStage,perl=TRUE)]
  g[,ExplicitCup:=grepl("cup|copa|kubok|karikas|kupa|coupe|pokal|beker",RSSSFSection,ignore.case=TRUE)]
  g[,Decision:=fcase(Existing,"PRESENT",AfterNames,"NAMING_EXPLAINS_MISMATCH",
    ExplicitCup,"COMPETITION_BOUNDARY_REVIEW",!NumberedRound,"NON_NUMBERED_STAGE_REVIEW",
    ClubDateConflict,"NAME_DATE_OR_SCORE_CONFLICT",default="POTENTIAL_MISSING_FIXTURE")]
  cat("[3/5] Ranking domestic gaps and extraction failures...\n")
  summary<-g[,.(ExtractedDated=.N,Present=sum(Existing),NamingExplained=sum(!Existing & AfterNames),
    PotentialMissing=sum(Decision=="POTENTIAL_MISSING_FIXTURE"),
    OtherReview=sum(!AfterNames & Decision!="POTENTIAL_MISSING_FIXTURE")),by=.(Country,Season)]
  audit<-fread(file.path(saved,"season_audit.csv"))
  audit[,Country:=country(Country)]
  audit<-audit[Country %in% seeds$Country & CompetitionType=="league" & StartYear>=2010L & StartYear<=2025L]
  audit<-audit[,.(RSSSFPlayedResults=sum(RSSSFPlayedResults),RSSSFDatedResults=sum(RSSSFDatedResults),
    ExtractionErrors=sum(nzchar(Error)),Errors=paste(unique(Error[nzchar(Error)]),collapse=" | "),
    SourceFiles=paste(unique(SourceFile),collapse=" | ")),by=.(Country,Season)]
  summary<-merge(audit,summary,by=c("Country","Season"),all=TRUE)
  summary<-merge(summary,d[,.(MasterMatches=.N),by=.(Country,Season)],by=c("Country","Season"),all.x=TRUE)
  for(col in c("ExtractedDated","Present","NamingExplained","PotentialMissing","OtherReview","MasterMatches","ExtractionErrors"))
    summary[is.na(get(col)),(col):=0L]
  summary<-merge(summary,seeds,by="Country",all.x=TRUE)
  summary[,Priority:=fcase(PotentialMissing>=20L,"MATERIAL_CANDIDATE_BATCH",
    ExtractionErrors>0L & MasterMatches==0L,"FAILED_EXTRACTION_NO_MASTER_SEASON",
    MasterMatches>ExtractedDated,"MASTER_EXCEEDS_SAVED_EXTRACTION",
    default="LOW_PRIORITY_OR_NAME_REVIEW")]
  setorder(summary,-PotentialMissing,Country,Season)
  fwrite(summary,file.path(out,"domestic_season_summary.csv"))
  fwrite(g[AfterNames==FALSE],file.path(out,"domestic_fixture_review.csv"))
  country_summary<-summary[,.(Seasons=.N,MasterMatches=sum(MasterMatches),ExtractionErrors=sum(ExtractionErrors),
    ExtractedDated=sum(ExtractedDated),Present=sum(Present),NamingExplained=sum(NamingExplained),
    PotentialMissing=sum(PotentialMissing),MaterialSeasons=sum(PotentialMissing>=20L)),by=.(Confederation,Country)]
  fwrite(country_summary,file.path(out,"domestic_country_summary.csv"))
  cat("[4/5] Comparing saved continental fixtures...\n")
  cpath<-file.path(base,"Manual_Sources/RSSSF_World_Audit/continental_rebuild/parsed_games.csv")
  if(file.exists(cpath)) {
    cg<-fread(cpath); cg[,Date:=as.IDate(Date)]
    cm<-m[CompetitionType=="continental" & Country %chin% c("Asia","Africa","North America","South America","Oceania")]
    confmap<-c("Asia"="AFC","Africa"="CAF","North America"="CONCACAF","South America"="CONMEBOL","Oceania"="OFC")
    cm[,Confederation:=unname(confmap[Country])]
    cm[,`:=`(H=norm(canon(HomeAssociation,Home)),A=norm(canon(AwayAssociation,Away)))]
    cg[,`:=`(H=norm(Home),A=norm(Away))]
    ckey<-function(z) paste(z$Confederation,z$Date,z$H,z$A,z$Score,sep="\r")
    cg[,ExactPresent:=ckey(cg) %chin% ckey(cm)]
    # Source-file/date/score is supporting evidence only: names/associations
    # still require review; never merge cross-country names automatically.
    fingerprint<-function(z) paste(z$Confederation,basename(z$SourceFile),z$Date,z$Score,sep="\r")
    cg[,SourceDateScorePresent:=fingerprint(cg) %chin% fingerprint(cm)]
    cs<-cg[,.(SavedFixtures=.N,ExactNameMatch=sum(ExactPresent),
      SourceDateScorePresent=sum(SourceDateScorePresent),
      NoSourceDateScoreMatch=sum(!SourceDateScorePresent)),by=.(Confederation,Season)]
    fwrite(cs,file.path(out,"continental_season_summary.csv"))
    fwrite(cg[ExactPresent==FALSE],file.path(out,"continental_fixture_review.csv"))
    print(cs[,.(SavedFixtures=sum(SavedFixtures),ExactNameMatch=sum(ExactNameMatch),
      SourceDateScorePresent=sum(SourceDateScorePresent)),by=Confederation])
  }
  cat("[5/5] Domestic overview:\n")
  print(country_summary[,.(Countries=.N,Seasons=sum(Seasons),ExtractionErrors=sum(ExtractionErrors),
    ExtractedDated=sum(ExtractedDated),Present=sum(Present),NamingExplained=sum(NamingExplained),
    PotentialMissing=sum(PotentialMissing),MaterialSeasons=sum(MaterialSeasons)),by=Confederation])
  print(summary[PotentialMissing>=20L][1:min(.N,20L),.(Country,Season,MasterMatches,ExtractedDated,PotentialMissing,Priority)])
  cat("Saved extraction may predate later country repairs. Master-exceeds-extraction flags are not proof of missing games.\n")
  cat("Dated share is not full-season coverage. Online cache gaps are not checked in this offline pass.\n")
  cat("No production changes, downloads or parsing. Report: ",out,"\n",sep="")
}
run_non_uefa_situation()
