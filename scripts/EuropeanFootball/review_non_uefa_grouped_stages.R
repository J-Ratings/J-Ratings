# Offline grouped-table diagnosis. Does not approve imports or change aliases.
# Compares saved results with each distinct final table; repeated tables are
# deduplicated. Aggregate tables are reported separately, never summed.
run_grouped_stage_review <- function() {
  suppressPackageStartupMessages(library(data.table))
  source("scripts/EuropeanFootball/rsssf_roster_abbreviations.R")
  on.exit(if(interactive() && requireNamespace("beepr",quietly=TRUE))
    try(beepr::beep(),silent=TRUE),add=TRUE)
  base<-file.path(normalizePath(getwd(),winslash="/",mustWork=TRUE),"EuropeanFootball/pipeline_data")
  saved<-file.path(base,"Manual_Sources/RSSSF_World_Refresh_Review")
  out<-file.path(saved,"grouped_stage_review")
  dir.create(out,recursive=TRUE,showWarnings=FALSE)
  b<-fread(file.path(saved,"new_roster_club_recheck/batch_summary.csv"))
  targets<-b[Country %chin% c("Peru","DR Congo") & OtherHeld>=20L]
  g<-fread(file.path(saved,"all_dated_rsssf_games.csv"),encoding="UTF-8")
  g<-merge(g,targets[,.(Country,Season)],by=c("Country","Season"))
  norm<-function(x) gsub("[^a-z0-9]","",tolower(stringi::stri_trans_general(x,"Latin-ASCII")))
  core<-function(x) norm(gsub("\\b(FC|SC|CF|FK|AFC)\\b","",gsub("\\([^)]*\\)","",x),ignore.case=TRUE,perl=TRUE))
  cleanline<-function(x) trimws(gsub("[[:space:]]+"," ",x))
  summaries<-list(); fixtures<-list(); rosters<-list()
  cat("[1/3] Reviewing ",nrow(targets)," saved grouped-season pages...\n",sep="")
  for(i in seq_len(nrow(targets))) {
    ct<-targets$Country[i]; ss<-targets$Season[i]; path<-targets$SourceFile[i]
    cat("  [",i,"/",nrow(targets),"] ",ct," ",ss,"\n",sep="")
    doc<-xml2::read_html(path)
    pre<-xml2::xml_find_first(doc,"//h4[1]/following::pre[1]")
    if(inherits(pre,"xml_missing")) pre<-xml2::xml_find_first(doc,"//pre[1]")
    lines<-strsplit(xml2::xml_text(pre),"\n",fixed=TRUE)[[1]]
    # Includes dash-separated P/W/D/L columns without confusing numeric names.
    sep<-"\\s+(?:[-\u2013\u2014\u0096]\\s*)?"
    rx<-paste0("^\\s*(?:[0-9]+|-)[.]\\s*(.*?)",sep,"([0-9]+)",sep,
      "([0-9]+)",sep,"([0-9]+)",sep,"([0-9]+)",sep,"[0-9]+\\s*[-:]\\s*[0-9]+")
    headers<-grep("^Final.*Table",trimws(lines),ignore.case=TRUE)
    tables<-list()
    for(j in seq_along(headers)) {
      start<-headers[j]
      finish<-if(j<length(headers)) headers[j+1L]-1L else length(lines)
      idx<-seq.int(start,finish)
      stop_at<-grep("^Round [0-9]+|^Topscorers|^Halfway Table|^NB:",trimws(lines[idx]))
      if(length(stop_at)) idx<-head(idx,min(stop_at)-1L)
      if(!length(idx)) next
      hit<-regmatches(lines[idx],regexec(rx,lines[idx],perl=TRUE))
      take<-which(lengths(hit)==6L)
      if(length(take)<4L) next
      tab<-rbindlist(lapply(hit[take],function(z) data.table(Name=z[2],
        P=as.integer(z[3]),W=as.integer(z[4]),D=as.integer(z[5]),L=as.integer(z[6]))))
      tab[,Key:=core(Name)]
      if(any(tab$P!=tab$W+tab$D+tab$L)||anyDuplicated(tab$Key)) next
      signature<-paste(sort(paste(tab$Key,tab$P)),collapse="|")
      if(length(tables) && signature %chin% vapply(tables,`[[`,character(1),"Signature")) next
      tables[[length(tables)+1L]]<-list(Start=start,Roster=tab,Signature=signature,
        Aggregate=grepl("aggregate",lines[start],ignore.case=TRUE))
    }
    pg<-g[Country==ct & Season==ss]
    pg<-pg[!(tolower(as.character(Annotated)) %chin% c("true","t","1"))]
    # Exact source-line evidence, not an inferred date/score fixture match.
    literal<-cleanline(lines)
    positions<-lapply(cleanline(pg$RawLine),function(x) which(literal==x))
    pg[,TextLine:=vapply(positions,function(x) if(length(x)==1L) x else NA_integer_,integer(1))]
    for(j in seq_along(tables)) {
      t<-tables[[j]]; tab<-t$Roster
      next_start<-if(j<length(tables)) tables[[j+1L]]$Start else length(lines)+1L
      membership<-function(x) {
        hit<-which(core(x)==tab$Key)
        if(length(hit)==1L) return(tab$Key[hit])
        hit<-rsssf_roster_abbreviation_match(x,tab$Name)
        if(length(hit)==1L) return(tab$Key[hit])
        k<-core(x)
        hit<-which(vapply(tab$Key,function(y) nchar(k)>=5L && nchar(y)>=5L &&
          (grepl(k,y,fixed=TRUE)||grepl(y,k,fixed=TRUE)),logical(1)))
        if(length(hit)==1L) tab$Key[hit] else NA_character_
      }
      roster_map<-setNames(vapply(unique(c(pg$RHome,pg$RAway)),membership,character(1)),unique(c(pg$RHome,pg$RAway)))
      pg[,`:=`(RH=unname(roster_map[RHome]),RA=unname(roster_map[RAway]))]
      q<-pg[!is.na(TextLine) & TextLine>t$Start & TextLine<next_start &
        !is.na(RH) & !is.na(RA) & RH!=RA & grepl("^Round [0-9]+",RSSSFStage)]
      q<-unique(q,by=c("Date","RH","RA","Score"))
      appearances<-rbind(q[,.(Club=RH,Date)],q[,.(Club=RA,Date)])
      expected<-sum(tab$P)/2
      count_ok<-expected==as.integer(expected) && nrow(q)>=.9*expected && nrow(q)<=expected
      schedule_ok<-!anyDuplicated(appearances)
      summaries[[length(summaries)+1L]]<-data.table(Country=ct,Season=ss,Table=j,
        TableHeading=trimws(lines[t$Start]),Aggregate=t$Aggregate,RosterTeams=nrow(tab),
        ExpectedGames=expected,LocatedRosterGames=nrow(q),RosterClubsObserved=uniqueN(appearances$Club),
        SameDayClash=!schedule_ok,Decision=if(t$Aggregate) "AGGREGATE_REFERENCE_ONLY" else
          if(count_ok && schedule_ok && uniqueN(appearances$Club)==nrow(tab)) "STAGE_STRUCTURE_PLAUSIBLE" else "STAGE_REVIEW",
        SourceFile=path)
      rosters[[length(rosters)+1L]]<-cbind(data.table(Country=ct,Season=ss,Table=j),tab)
      if(nrow(q)) { q[,ReviewTable:=j]; fixtures[[length(fixtures)+1L]]<-copy(q) }
    }
    if(!length(tables)) summaries[[length(summaries)+1L]]<-data.table(Country=ct,Season=ss,
      Decision="NO_VALID_GROUP_TABLE",SourceFile=path)
  }
  cat("[2/3] Writing stage comparisons and roster evidence...\n")
  report<-rbindlist(summaries,fill=TRUE)
  fwrite(report,file.path(out,"stage_summary.csv"),na="")
  fwrite(rbindlist(rosters,fill=TRUE),file.path(out,"stage_rosters.csv"),na="")
  fwrite(rbindlist(fixtures,fill=TRUE),file.path(out,"located_stage_fixtures.csv"),na="")
  cat("[3/3] Grouped-stage review:\n")
  print(report[,.(Country,Season,Table,Aggregate,ExpectedGames,LocatedRosterGames,Decision)])
  cat("Stage counts are diagnostic, not approved imports. Source boundaries and identities still need validation.\n")
  cat("No downloads, aliases or production changes. Report: ",out,"\n",sep="")
}
run_grouped_stage_review()
