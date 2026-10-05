# Offline review only. No production files are modified.
run_identity_source_validation <- function() {
  suppressPackageStartupMessages(library(data.table))
  stopifnot(requireNamespace("xml2",quietly=TRUE),requireNamespace("stringi",quietly=TRUE))
  started<-Sys.time()
  on.exit({cat("Elapsed:",round(as.numeric(difftime(Sys.time(),started,units="secs"))),"seconds\n")
    if(interactive()&&requireNamespace("beepr",quietly=TRUE)) try(beepr::beep(),silent=TRUE)},add=TRUE)
  base<-file.path(normalizePath(getwd(),winslash="/",mustWork=TRUE),"EuropeanFootball/pipeline_data")
  prior<-file.path(base,"Manual_Sources/Team_Identity_Audit/systematic_split_review")
  out<-file.path(prior,"source_validation");dir.create(out,recursive=TRUE,showWarnings=FALSE)
  norm<-function(x) trimws(gsub(" +"," ",gsub("[^a-z0-9 ]"," ",
    tolower(stringi::stri_trans_general(x,"Latin-ASCII")))))
  cat("[1/4] Reading master and selected identity pairs...\n")
  m<-fread(file.path(base,"Matches_Clean_Combined/european_football_all_matches.csv"),encoding="UTF-8")
  m[,Row:=.I];m[,Date:=as.IDate(Date)]
  pairs<-fread(file.path(prior,"identity_pairs_review.csv"),encoding="UTF-8")
  pairs<-pairs[Priority!="4_HISTORICAL_NAME_REVIEW"]
  aliases<-fread(file.path(base,"Reference/team_aliases.csv"),encoding="UTF-8")
  aliases<-aliases[,if(uniqueN(CanonicalName)==1L) .(CanonicalName=CanonicalName[1]) else NULL,by=.(Country,SourceName)]
  amap<-setNames(aliases$CanonicalName,paste(tolower(aliases$Country),aliases$SourceName,sep="|"))
  canon<-function(c,n) {
    for(i in 1:10) {z<-unname(amap[paste(tolower(c),n,sep="|")]);u<-!is.na(z)&z!=n
      if(!any(u))break
    n[u]<-z[u]};n
  }
  site<-as.data.table(jsonlite::fromJSON(file.path(dirname(base),"data/teams.json")))
  site[,ResolvedName:=canon(country,name)]
  # Existing aliases may form chains that 02 applies only one step at a time.
  site[,OneStepName:=unname(amap[paste(tolower(country),name,sep="|")])]
  fwrite(site[ResolvedName!=name],file.path(out,"published_names_with_existing_aliases.csv"),na="")
  chain<-copy(aliases)
  chain[,FinalName:=canon(Country,CanonicalName)]
  fwrite(chain[FinalName!=CanonicalName],file.path(out,"existing_alias_chains.csv"),na="")
  hc<-m$Country;ac<-m$Country
  ct<-which(m$CompetitionType=="continental")
  hc[ct]<-m$HomeAssociation[ct];ac[ct]<-m$AwayAssociation[ct]
  m[,`:=`(H=paste(tolower(hc),canon(hc,Home),sep="|"),A=paste(tolower(ac),canon(ac,Away),sep="|"))]
  approx<-tolower(as.character(m$DateApprox)) %chin% c("true","t","1","yes")
  cat("[2/4] Testing identity pairs against actual schedules...\n")
  schedules<-unique(rbind(m[!approx,.(Identity=H,Date,Opponent=A,Score,Side="H",Row)],
    m[!approx,.(Identity=A,Date,Opponent=H,Score,Side="A",Row)]))
  details<-list();examples<-list()
  for(i in seq_len(nrow(pairs))) {
    pa<-pairs[i];x<-schedules[Identity==pa$A];y<-schedules[Identity==pa$B]
    shared<-merge(x,y,by=c("Date","Opponent","Score","Side"),allow.cartesian=TRUE)
    h2h<-m[(H==pa$A&A==pa$B)|(H==pa$B&A==pa$A)]
    jan1<-sum(format(shared$Date,"%m-%d")=="01-01")
    details[[i]]<-data.table(A=pa$A,B=pa$B,NameA=pa$NameA,NameB=pa$NameB,
      CountryA=pa$CountryA,CountryB=pa$CountryB,Reasons=pa$Reasons,
      ExactFixtureDates=uniqueN(shared$Date),NonJanuary1FixtureDates=uniqueN(shared$Date[format(shared$Date,"%m-%d")!="01-01"]),
      HeadToHeadMatches=nrow(h2h),January1EvidenceRows=jan1,
      Decision=if(nrow(h2h)) "SEPARATE_OR_SOURCE_ERROR_REVIEW" else if(nrow(shared)&&jan1==nrow(shared)) "PLACEHOLDER_DATE_REVIEW" else "IDENTITY_NEEDS_SOURCE_CONFIRMATION")
    if(nrow(shared))examples[[length(examples)+1L]]<-shared[,.(PairA=pa$A,PairB=pa$B,Date,Opponent,Score,Side,RowA=Row.x,RowB=Row.y)]
    if(i%%25L==0L||i==nrow(pairs))cat("  Identity pairs:",i,"/",nrow(pairs),"\n")
  }
  fwrite(rbindlist(details),file.path(out,"identity_validation.csv"),na="")
  fwrite(if(length(examples))rbindlist(examples) else data.table(Note=character()),file.path(out,"fixture_evidence.csv"),na="")
  cat("[3/4] Tracing league rows to cached HTML sections, country by country...\n")
  league<-m[CompetitionType=="league"&!is.na(SourceFile)&nzchar(SourceFile)]
  league[,`:=`(HomeNorm=norm(Home),AwayNorm=norm(Away))]
  files<-unique(league$SourceFile)
  selected<-Sys.getenv("IDENTITY_SECTION_COUNTRIES","")
  if(nzchar(selected))files<-unique(league[Country %chin% strsplit(selected,";",fixed=TRUE)[[1]],SourceFile])
  findings<-list();page_audit<-list()
  for(fi in seq_along(files)) {
    f<-files[fi];rows<-league[SourceFile==f]
    result<-tryCatch({
      doc<-xml2::read_html(f)
      # Follow the DOM order, rather than link text from the contents list.
      nodes<-xml2::xml_find_all(doc,"//h1|//h2|//h3|//h4|//h5|//h6|//pre")
      section<-"";chunks<-list()
      for(ni in seq_along(nodes)) {
        node<-nodes[[ni]];tag<-xml2::xml_name(node);tx<-xml2::xml_text(node)
        if(tag!="pre") {section<-trimws(tx);next}
        lines<-strsplit(tx,"\n",fixed=TRUE)[[1]]
        # Preserve headings embedded inside a single preformatted block.
        current<-section
        for(li in seq_along(lines)) {
          line<-trimws(lines[li])
          is_score<-grepl("[0-9]+[[:space:]]*[-:][[:space:]]*[0-9]+",line)
          heading<-!is_score&&nchar(line)<120L&&grepl("cup|championship|premier|division|league|women|reserve",line,ignore.case=TRUE)&&
            !grepl("^[0-9]|^NB:|^Note|^\\[",line)
          if(heading)current<-paste(section,line,sep=" / ")
          if(is_score)chunks[[length(chunks)+1L]]<-data.table(Section=current,Line=line,LineNorm=norm(line))
        }
      }
      if(!length(chunks))return_value<-data.table() else return_value<-unique(rbindlist(chunks))
      return_value
    },error=function(e)e)
    if(inherits(result,"error")) {page_audit[[fi]]<-data.table(SourceFile=f,Rows=nrow(rows),Status=conditionMessage(result));next}
    suspect<-if(nrow(result))result[grepl("cup|canadian championship|women|reserve|under.?([12][0-9])",Section,ignore.case=TRUE)] else data.table()
    matched<-0L
    if(nrow(suspect))for(ri in seq_len(nrow(rows))) {
      rr<-rows[ri]
      if(is.na(rr$HomeNorm)||is.na(rr$AwayNorm)||!nzchar(rr$HomeNorm)||!nzchar(rr$AwayNorm)||is.na(rr$Score))next
      score<-gsub(" ","",rr$Score,fixed=TRUE)
      lines<-suspect[grepl(rr$HomeNorm,LineNorm,fixed=TRUE)&grepl(rr$AwayNorm,LineNorm,fixed=TRUE)]
      if(nrow(lines))lines<-lines[grepl(score,gsub(" ","",Line,fixed=TRUE),fixed=TRUE)]
      if(!nrow(lines))next
      matched<-matched+1L
      findings[[length(findings)+1L]]<-data.table(Row=rr$Row,Country=rr$Country,Season=rr$Season,Date=rr$Date,
        Home=rr$Home,Away=rr$Away,Score=rr$Score,Competition=rr$Competition,Type=rr$CompetitionType,
        SourceFile=f,Sections=paste(unique(lines$Section),collapse="; "),
        SourceLines=paste(unique(lines$Line),collapse="; "),
        Decision="SOURCE_SECTION_REVIEW_NOT_APPROVED")
    }
    page_audit[[fi]]<-data.table(SourceFile=f,Rows=nrow(rows),SuspectSectionMatches=matched,Status="SCANNED")
    if(fi%%25L==0L||fi==length(files))cat("  Source pages:",fi,"/",length(files),";",rows$Country[1],"\n")
  }
  cat("[4/4] Writing review reports...\n")
  fwrite(rbindlist(page_audit,fill=TRUE),file.path(out,"source_scan_status.csv"),na="")
  flags<-if(length(findings))unique(rbindlist(findings)) else data.table(Country=character(),Row=integer())
  fwrite(flags,file.path(out,"suspect_league_source_sections.csv"),na="")
  if(nrow(flags))print(flags[,.(FlaggedRows=.N,Seasons=uniqueN(Season)),by=Country][order(-FlaggedRows)])
  writeLines(c("All outputs are review-only; no aliases or master rows changed.",
    "A source-line name/score match does not establish the date or uniquely identify a repeated fixture.",
    "Cup words in headings may describe legitimate league playoff qualification; inspect source sections.",
    "Unmatched rows may have canonicalised names or unsupported HTML layout: absence of a flag is not clearance.",
    "DateApprox rows excluded from fixture evidence; January 1 evidence separately flagged.",
    "Head-to-head fixtures are evidence against merging, but can also be incorrectly parsed rows."),file.path(out,"READ_ME.txt"))
  cat("No production changes or downloads. Report:",out,"\n")
}
run_identity_source_validation()
