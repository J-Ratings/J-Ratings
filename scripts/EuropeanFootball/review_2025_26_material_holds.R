# Consolidated offline evidence review. Never applies aliases or imports games.
review_target_material_holds <- function() {
  suppressPackageStartupMessages({library(data.table);library(stringi)})
  base<-"EuropeanFootball/pipeline_data";batch<-file.path(base,"Manual_Sources/Season_2025_26")
  out<-file.path(batch,"material_hold_review");dir.create(out,recursive=TRUE,showWarnings=FALSE)
  cat("[1/4] Reading held fixtures and current domestic names...\n")
  h<-fread(file.path(batch,"import_validation/held_or_present.csv"),encoding="UTF-8")
  evidence<-fread(file.path(batch,"identity_hold_review/identity_evidence.csv"),encoding="UTF-8")
  m<-fread(file.path(base,"Matches_Clean_Combined/european_football_all_matches.csv"),
    select=c("Country","CompetitionType","Tier","Competition","League","Season","Home","Away"),encoding="UTF-8")
  a<-fread(file.path(base,"Reference/team_aliases.csv"),encoding="UTF-8")
  source("scripts/EuropeanFootball/club_identity_resolution.R",local=TRUE)
  lm<-setNames(a$CanonicalName,paste(a$Country,a$SourceName,sep="\r"));rm<-setNames(character(),character())
  clean<-function(n)trimws(gsub(" +"," ",gsub("[^a-z0-9]"," ",tolower(stri_trans_general(n,"Latin-ASCII")))))
  tokens<-function(n)setdiff(strsplit(clean(n)," ",fixed=TRUE)[[1]],
    c("fc","cf","afc","fk","kf","sk","sc","ac","club","clube","football","futebol","futbol","esporte","esportiva","sociedade","sporting","regatas","de","da","do","e","the"))
  clubs<-unique(rbind(m[CompetitionType=="league",.(Country,Name=Home)],m[CompetitionType=="league",.(Country,Name=Away)]))
  clubs[,Name:=resolve_alias_chain(Name,Country,lm,rm)];clubs<-unique(clubs)
  names_to_check<-evidence[IdentityDecision %in% c("TWO_SOURCE_NEW_ROSTER_CLUB_REVIEW","POSSIBLE_HISTORICAL_VARIANT")]
  cat("[2/4] Comparing historical club words without fuzzy merges...\n")
  result<-list()
  for(i in seq_len(nrow(names_to_check))) {
    x<-names_to_check[i];known<-clubs[Country==x$Country]
    wt<-tokens(x$WikiName);st<-tokens(x$SourceName)
    hits<-character()
    for(name in known$Name) {
      kt<-tokens(name)
      matches<-function(t)length(t)>0L&&length(kt)>0L&&nchar(paste(t,collapse=""))>=4L&&
        nchar(paste(kt,collapse=""))>=4L&&(all(t %in% kt)||all(kt %in% t))
      if(matches(wt)||matches(st))hits<-c(hits,name)
    }
    x[,`:=`(HistoricalCoreCandidates=paste(sort(unique(hits)),collapse="; "),
      CoreCandidateCount=length(unique(hits)),ReviewClass=if(length(hits)==1L)"ONE_HISTORICAL_CORE_CANDIDATE"else
        if(length(hits)>1L)"MULTIPLE_HISTORICAL_CORE_CANDIDATES"else "NO_WORD_MATCH_NOT_PROOF_OF_NEW_CLUB")]
    result[[i]]<-x
  }
  identities<-rbindlist(result,fill=TRUE)
  apps<-rbind(h[Validation=="UNKNOWN_OR_CHANGED_CLUB_IDENTITY_REVIEW",.(Country,SourceName=Home,WikiName=MatchedHome,CandidateID)],
    h[Validation=="UNKNOWN_OR_CHANGED_CLUB_IDENTITY_REVIEW",.(Country,SourceName=Away,WikiName=MatchedAway,CandidateID)])
  appearances<-apps[,.(HeldFixtureAppearances=uniqueN(CandidateID)),by=.(Country,SourceName,WikiName)]
  identities<-merge(identities,appearances,by=c("Country","SourceName","WikiName"),all.x=TRUE)
  setorder(identities,-HeldFixtureAppearances,Country)
  fwrite(identities,file.path(out,"historical_identity_candidates.csv"))
  cat("[3/4] Separating annual split stages from regional competitions...\n")
  stages<-h[Validation=="MULTI_COMPETITION_STAGE_REVIEW",. (HeldMatches=.N,FirstDate=min(Date),LastDate=max(Date),
    WikiStages=paste(sort(unique(WikiStage)),collapse="; ")),by=.(Country,Code,CompetitionLabel,Season)]
  stages[,ReviewClass:=fifelse(grepl("Apertura|Clausura|Finalizaci",CompetitionLabel,ignore.case=TRUE),
    "ANNUAL_TOP_FLIGHT_STAGE_MAPPING",fifelse(Country=="New Zealand","REGIONAL_AND_NATIONAL_STAGE_MAPPING","SOURCE_SCOPE_MAPPING"))]
  fwrite(stages,file.path(out,"competition_stage_candidates.csv"))
  production<-m[CompetitionType=="league"&Tier==1&Country %in% stages$Country,
    .(Rows=.N,LatestSeason=tail(sort(unique(Season)),1L)),by=.(Country,Competition,League)]
  fwrite(production,file.path(out,"existing_competition_labels.csv"))
  # Dates/scores that disagree remain visible rather than discarded.
  fwrite(h[Validation %in% c("MASTER_CLUB_DATE_CONFLICT_REVIEW","NEARBY_EXISTING_FIXTURE_REVIEW",
    "EXISTING_FIXTURE_SCORE_REVIEW","SEASON_DATE_REVIEW")],file.path(out,"date_score_conflicts.csv"))
  cat("[4/4] Consolidated overview:\n");print(identities[,.N,by=ReviewClass]);print(stages[,.(HeldMatches=sum(HeldMatches)),by=.(Country,ReviewClass)])
  cat("These are evidence candidates, not approved aliases or imports.\nReport:",normalizePath(out,winslash="/"),"\n")
}
review_target_material_holds()
