# Offline evidence proposals only. Never applies a name mapping.
review_cached_subject_variants <- function() {
  suppressPackageStartupMessages({library(data.table);library(xml2);library(stringi)})
  started<-Sys.time()
  on.exit({cat("Elapsed:",round(as.numeric(difftime(Sys.time(),started,units="secs"))),"seconds\n")
    if(interactive()&&requireNamespace("beepr",quietly=TRUE))try(beepr::beep(),silent=TRUE)},add=TRUE)
  batch<-"EuropeanFootball/pipeline_data/Manual_Sources/Season_2025_26"
  out<-file.path(batch,"remaining_identity_review")
  r<-fread(file.path(out,"club_review.csv"),encoding="UTF-8")
  names<-fread(file.path(out,"historical_name_suggestions.csv"),encoding="UTF-8")
  articles<-fread(file.path(batch,"club_article_evidence/club_article_review.csv"),encoding="UTF-8")
  clean<-function(x)trimws(gsub(" +"," ",gsub("[^a-z0-9]"," ",tolower(stri_trans_general(x,"Latin-ASCII")))))
  evidence<-list()
  cat("Checking cached club subjects for",nrow(r),"unresolved labels; no web requests.\n")
  for(i in seq_len(nrow(r))) {
    if(i==1L||i%%20L==0L||i==nrow(r))cat(sprintf("  Club %d/%d\n",i,nrow(r)))
    row<-r[i]
    path<-file.path(batch,"club_article_evidence/cache",paste0(gsub("[^A-Za-z0-9]","_",paste(row$Country,row$WikiName,sep="_")),".html"))
    z<-data.table(Country=row$Country,SourceName=row$SourceName,WikiName=row$WikiName,
      BlockedMatches=row$BlockedMatches,ProposedExistingName="",SubjectNames="",ReviewDecision="NO_CACHED_ARTICLE",ArticleURL=row$ArticleURL)
    if(file.exists(path))tryCatch({
      page<-read_html(path)
      section<-xml_find_first(page,"//section[@data-mw-section-id='0']")
      subjects<-if(!inherits(section,"xml_missing"))xml_text(xml_find_all(section,"./p//b | .//table[contains(@class,'infobox')]//th[contains(@class,'fn')]")) else
        xml_text(xml_find_all(page,"//div[contains(@class,'mw-parser-output')]/p//b | //table[contains(@class,'infobox')]//th[contains(@class,'fn')]"))
      subjects<-unique(subjects[nzchar(trimws(subjects))])
      z[,SubjectNames:=paste(subjects,collapse="; ")]
      country<-row$Country;wiki<-row$WikiName;src<-row$SourceName
      candidates<-unique(names[Country==country & WikiName==wiki & SourceName==src,CandidateName])
      hits<-candidates[vapply(candidates,function(n){
        text<-clean(n)
        if(grepl("\\breserves?\\b|\\byouth\\b|\\bu[0-9]{2}\\b",clean(row$WikiName)) &&
          !grepl("\\breserves?\\b|\\byouth\\b|\\bu[0-9]{2}\\b",text))return(FALSE)
        # A complete historical name must occur as whole words in the subject,
        # not in opponents, history paragraphs or fuzzy similarity scores.
        nchar(gsub(" ","",text))>=5L && any(grepl(paste0(" ",text," "),paste0(" ",clean(subjects)," "),fixed=TRUE))
      },logical(1))]
      z[,`:=`(ProposedExistingName=paste(hits,collapse="; "),ReviewDecision=if(length(hits)==1L)
        "UNIQUE_HISTORICAL_NAME_IN_ARTICLE_SUBJECT_REVIEW" else if(length(hits)>1L)
        "AMBIGUOUS_ARTICLE_SUBJECT_REVIEW" else "NO_EXISTING_SUBJECT_NAME_MATCH")]
    },error=function(e)z[,ReviewDecision:=paste("ARTICLE_PARSE_ERROR",conditionMessage(e))])
    evidence[[i]]<-z
  }
  result<-rbindlist(evidence)
  setorder(result,-BlockedMatches)
  fwrite(result,file.path(out,"cached_subject_variant_review.csv"),na="")
  print(result[,.N,by=ReviewDecision])
  print(result[ReviewDecision=="UNIQUE_HISTORICAL_NAME_IN_ARTICLE_SUBJECT_REVIEW",
    .(Country,WikiName,ProposedExistingName,BlockedMatches)])
  cat("These are proposals for review, not aliases or verified new clubs. Master unchanged.\nReport:",normalizePath(out,winslash="/"),"\n")
}
review_cached_subject_variants()
