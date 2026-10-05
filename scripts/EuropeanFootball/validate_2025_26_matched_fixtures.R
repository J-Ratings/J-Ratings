# Offline validation. Produces an import preview; never writes the master.
validate_target_fixtures <- function() {
  suppressPackageStartupMessages({library(data.table);library(stringi)})
  started<-Sys.time()
  on.exit({cat("Elapsed:",round(as.numeric(difftime(Sys.time(),started,units="secs"))),"seconds\n")
    if(interactive()&&requireNamespace("beepr",quietly=TRUE))try(beepr::beep(),silent=TRUE)},add=TRUE)
  base<-"EuropeanFootball/pipeline_data"
  input<-file.path(base,"Manual_Sources/Season_2025_26/club_label_recheck")
  out<-file.path(base,"Manual_Sources/Season_2025_26/import_validation")
  dir.create(out,recursive=TRUE,showWarnings=FALSE)
  cat("[1/5] Reading matched candidates and current master...\n")
  x<-fread(file.path(input,"fixture_review.csv"),encoding="UTF-8")
  x<-x[Decision=="UNIQUE_SCORE_DATE_MATCH_REVIEW"]
  x[,CandidateID:=seq_len(.N)]
  master_path<-file.path(base,"Matches_Clean_Combined/european_football_all_matches.csv")
  m<-fread(master_path,encoding="UTF-8",showProgress=FALSE)
  a<-fread(file.path(base,"Reference/team_aliases.csv"),encoding="UTF-8")
  source("scripts/EuropeanFootball/club_identity_resolution.R",local=TRUE)
  lm<-setNames(a$CanonicalName,paste(a$Country,a$SourceName,sep="\r"));rm<-setNames(character(),character())
  canon<-function(name,country)resolve_alias_chain(name,country,lm,rm)
  norm<-function(name)gsub("[^a-z0-9]","",tolower(stri_trans_general(name,"Latin-ASCII")))
  m[,`:=`(Date=as.IDate(Date),CH=canon(Home,Country),CA=canon(Away,Country))]
  x[,`:=`(Date=as.IDate(Date),CanonicalHome=canon(MatchedHome,Country),CanonicalAway=canon(MatchedAway,Country),Validation="VALIDATED_IMPORT_PREVIEW")]
  bridge_path<-file.path(base,"Manual_Sources/Season_2025_26/identity_hold_review/supported_source_bridges.csv")
  if(file.exists(bridge_path)) {
    bridges<-fread(bridge_path,encoding="UTF-8")
    if(any(!bridges$IdentityDecision %chin% c("EXACT_EXISTING_SOURCE_LABEL_BRIDGE","EXACT_CLUB_ARTICLE_SUBJECT_BRIDGE")))stop("Unexpected identity bridge decision")
    bridge_keys<-paste(bridges$Country,bridges$SourceName,bridges$WikiName,sep="|")
    if(anyDuplicated(bridge_keys))stop("Repeated source identity bridge")
    bm<-setNames(bridges$CanonicalName,bridge_keys)
    hv<-unname(bm[paste(x$Country,x$Home,x$MatchedHome,sep="|")])
    av<-unname(bm[paste(x$Country,x$Away,x$MatchedAway,sep="|")])
    x[!is.na(hv),CanonicalHome:=hv[!is.na(hv)]]
    x[!is.na(av),CanonicalAway:=av[!is.na(av)]]
    cat("  Batch-scoped source-label bridges:",nrow(bridges),"; global aliases unchanged.\n")
  }
  cat("[2/5] Checking country-scoped identities and fixture keys...\n")
  # Existing identities must match a unique known canonical name in this country.
  # Unknown new clubs and unresolved historical variants remain held.
  clubs<-unique(rbind(m[CompetitionType=="league",.(Country,Name=CH)],m[CompetitionType=="league",.(Country,Name=CA)]))
  clubs[,NK:=norm(Name)]
  collision<-clubs[,.(Names=uniqueN(Name)),by=.(Country,NK)][Names>1L]
  identity<-clubs[,.(Canonical=Name[1],Names=uniqueN(Name)),by=.(Country,NK)]
  lookup<-setNames(identity$Canonical,paste(identity$Country,identity$NK,sep="|"))
  x[,`:=`(HK=norm(CanonicalHome),AK=norm(CanonicalAway))]
  knownh<-unname(lookup[paste(x$Country,x$HK,sep="|")]);knowna<-unname(lookup[paste(x$Country,x$AK,sep="|")])
  x[is.na(knownh)|is.na(knowna),Validation:="UNKNOWN_OR_CHANGED_CLUB_IDENTITY_REVIEW"]
  bad<-paste(collision$Country,collision$NK,sep="|")
  x[paste(Country,HK,sep="|") %chin% bad|paste(Country,AK,sep="|") %chin% bad,Validation:="COLLIDING_EXISTING_IDENTITIES_REVIEW"]
  x[!is.na(knownh),CanonicalHome:=knownh[!is.na(knownh)]]
  x[!is.na(knowna),CanonicalAway:=knowna[!is.na(knowna)]]
  x[is.na(Date)|!grepl("^[0-9]+-[0-9]+$",Score)|HK==AK|nchar(CanonicalHome)>200L|nchar(CanonicalAway)>200L,
    Validation:="INVALID_DATE_SCORE_OR_CLUB"]
  x[Season=="2025" & (Date<as.IDate("2025-01-01")|Date>as.IDate("2025-12-31")),Validation:="SEASON_DATE_REVIEW"]
  x[Season=="2025/2026" & (Date<as.IDate("2025-07-01")|Date>as.IDate("2026-06-30")),Validation:="SEASON_DATE_REVIEW"]
  x[,FixtureKey:=paste(Country,Date,HK,AK,sep="|")]
  m[,FixtureKey:=paste(Country,Date,norm(CH),norm(CA),sep="|")]
  present<-m[,.(Scores=paste(sort(unique(Score)),collapse=";")),by=FixtureKey]
  scoremap<-setNames(present$Scores,present$FixtureKey)
  oldscore<-unname(scoremap[x$FixtureKey])
  x[!is.na(oldscore)&oldscore==Score,Validation:="ALREADY_PRESENT"]
  x[!is.na(oldscore)&oldscore!=Score,Validation:="EXISTING_FIXTURE_SCORE_REVIEW"]
  duplicates<-x[,.(N=.N),by=FixtureKey][N>1L,FixtureKey]
  x[FixtureKey %chin% duplicates & Validation=="VALIDATED_IMPORT_PREVIEW",Validation:="DUPLICATE_CANDIDATE_REVIEW"]
  cat("[3/5] Checking nearby dates, club schedules and competition scope...\n")
  # Similar existing fixtures on nearby dates can be date disagreements.
  m[,PairScore:=paste(Country,norm(CH),norm(CA),Score,sep="|")]
  x[,PairScore:=paste(Country,HK,AK,Score,sep="|")]
  nearby<-merge(x[Validation=="VALIDATED_IMPORT_PREVIEW",.(CandidateID,PairScore,CandidateDate=Date)],
    m[Date>=as.IDate("2024-12-25"),.(PairScore,MasterDate=Date)],by="PairScore",allow.cartesian=TRUE)
  nearby<-nearby[abs(as.integer(CandidateDate-MasterDate))<=7L]
  fwrite(nearby,file.path(out,"nearby_date_evidence.csv"))
  x[CandidateID %in% nearby$CandidateID,Validation:="NEARBY_EXISTING_FIXTURE_REVIEW"]
  apps<-unique(rbind(m[,.(Country,Date,Club=norm(CH))],m[,.(Country,Date,Club=norm(CA))]))
  used<-paste(apps$Country,apps$Date,apps$Club,sep="|")
  x[Validation=="VALIDATED_IMPORT_PREVIEW" & (paste(Country,Date,HK,sep="|") %chin% used|
    paste(Country,Date,AK,sep="|") %chin% used),Validation:="MASTER_CLUB_DATE_CONFLICT_REVIEW"]
  freshapps<-rbind(x[Validation=="VALIDATED_IMPORT_PREVIEW",.(CandidateID,Country,Date,Club=HK)],
    x[Validation=="VALIDATED_IMPORT_PREVIEW",.(CandidateID,Country,Date,Club=AK)])
  clashes<-freshapps[,.(N=uniqueN(CandidateID)),by=.(Country,Date,Club)][N>1L]
  conflictids<-merge(freshapps,clashes,by=c("Country","Date","Club"))$CandidateID
  x[CandidateID %in% conflictids,Validation:="CANDIDATE_CLUB_DATE_CONFLICT_REVIEW"]
  # Annual split stages and parallel regional competitions require explicit
  # competition mapping rather than choosing a country's most common label.
  sourcepages<-fread(file.path(base,"Manual_Sources/Season_2025_26/source_discovery/dated_source_checks.csv"))
  multi<-sourcepages[Status=="DATED_RESULTS_EXTRACTABLE_NOT_YET_MATCHED",.(N=uniqueN(Path)),by=Country][N>1L,Country]
  # Exact source/stage pairs for successive phases of the same national league.
  # Regional qualifying leagues (including New Zealand) remain held.
  stage_rules<-data.table(
    Country=c("Colombia","Colombia","Mexico","Mexico","Paraguay","Paraguay","Malta","Malta","Malta"),
    Code=c("COL1","COLP","MEX1","MEXA","PR1A","PR1C","MT1N","MT1N","MT1N"),
    WikiStage=c("Torneo Finalización / First stage / Results","Torneo Apertura / First stage / Results",
      "Torneo Clausura / Regular phase / Results","Torneo Apertura / Regular phase / Results",
      "Torneo Apertura / Results / NA","Torneo Clausura / Results / NA",
      "Opening round / First phase / Results","Opening round / Second phase / Play-Out","Opening round / Second phase / Top Six"),
    First=c("2025-07-01","2025-01-01","2026-01-01","2025-07-01","2025-01-01","2025-07-01","2025-07-01","2025-07-01","2025-07-01"),
    Last=c("2025-12-31","2025-06-30","2026-06-30","2025-12-31","2025-06-30","2025-12-31","2026-01-31","2026-01-31","2026-01-31"))
  stage_keys<-paste(stage_rules$Country,stage_rules$Code,stage_rules$WikiStage,sep="|")
  rule_index<-match(paste(x$Country,x$Code,x$WikiStage,sep="|"),stage_keys)
  stage_ok<-!is.na(rule_index) & x$Date>=as.IDate(stage_rules$First[rule_index]) & x$Date<=as.IDate(stage_rules$Last[rule_index])
  stage_ok[is.na(stage_ok)]<-FALSE
  x[Country %in% multi & !stage_ok & Validation=="VALIDATED_IMPORT_PREVIEW",Validation:="MULTI_COMPETITION_STAGE_REVIEW"]
  x[Validation=="VALIDATED_IMPORT_PREVIEW" & grepl("\\bcup\\b|promotion|relegation playoff|\\bfinal\\b",WikiStage,ignore.case=TRUE),
    Validation:="WIKIPEDIA_STAGE_SCOPE_REVIEW"]
  # Reuse a production competition only if exactly one recent league key exists.
  competitions<-unique(m[CompetitionType=="league"&Tier==1&Date>=as.IDate("2024-01-01"),.(Country,Competition)])
  map<-competitions[,.(ProductionCompetition=Competition[1],Choices=.N),by=Country]
  x<-merge(x,map,by="Country",all.x=TRUE)
  x[(is.na(Choices)|Choices!=1L)&Validation=="VALIDATED_IMPORT_PREVIEW",Validation:="PRODUCTION_COMPETITION_MAPPING_REVIEW"]
  cat("[4/5] Writing master-compatible preview and all held rows...\n")
  ready<-x[Validation=="VALIDATED_IMPORT_PREVIEW"]
  preview<-copy(ready)
  preview[,`:=`(Home=CanonicalHome,Away=CanonicalAway,Competition=ProductionCompetition,
    CompetitionType="league",Tier=1L,League=CompetitionLabel,Source="wikipedia_transfermarkt",
    SourcePage=SourceURL,Stage=WikiStage,DateApprox=FALSE,SourceFile="",HomeAssociation=Country,AwayAssociation=Country)]
  if(nrow(preview)) {
    sc<-tstrsplit(preview$Score,"-",fixed=TRUE)
    preview[,Result:=fifelse(as.integer(sc[[1]])>as.integer(sc[[2]]),"1-0",fifelse(as.integer(sc[[1]])<as.integer(sc[[2]]),"0-1","0.5-0.5"))]
  } else preview[,Result:=character()]
  for(col in setdiff(names(m),names(preview)))set(preview,j=col,value=NA)
  # Keep only the actual master schema plus validation provenance.
  schema<-names(fread(master_path,nrows=0))
  fwrite(preview[,c(schema,"CandidateID","Validation","Code","MatchedHome","MatchedAway"),with=FALSE],file.path(out,"validated_import_preview.csv"),na="")
  fwrite(x,file.path(out,"all_decisions.csv"),na="")
  fwrite(x[Validation!="VALIDATED_IMPORT_PREVIEW"],file.path(out,"held_or_present.csv"),na="")
  fwrite(x[,.N,by=.(Country,Season,Validation)],file.path(out,"country_summary.csv"))
  inputs<-c(master_path,file.path(base,"Reference/team_aliases.csv"),file.path(input,"fixture_review.csv"),file.path(out,"validated_import_preview.csv"),"scripts/EuropeanFootball/validate_2025_26_matched_fixtures.R")
  if(file.exists(bridge_path))inputs<-c(inputs,bridge_path)
  fwrite(data.table(Path=inputs,MD5=unname(tools::md5sum(inputs))),file.path(out,"validation_manifest.csv"))
  writeLines(c("Offline validation only. Master and aliases unchanged.",
    "Unknown clubs, existing-name collisions, nearby dates, club schedule conflicts and ambiguous competition keys are held.",
    "Unique score agreement does not establish full-season completeness.",
    "Do not use prior non-UEFA import scripts on this preview; this batch requires its own revalidated importer."),file.path(out,"READ_ME.txt"))
  cat("[5/5] Validation overview:\n");print(x[,.N,by=Validation][order(-N)])
  cat("Preview additions:",nrow(preview),". No production changes.\nReport:",normalizePath(out,winslash="/"),"\n")
}
validate_target_fixtures()
