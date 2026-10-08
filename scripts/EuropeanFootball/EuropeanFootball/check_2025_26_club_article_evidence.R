# Read club articles linked by cached season pages. Evidence only, no merges.
check_target_club_articles <- function() {
  suppressPackageStartupMessages({library(data.table);library(xml2);library(stringi)})
  started<-Sys.time()
  on.exit({cat("Elapsed:",round(as.numeric(difftime(Sys.time(),started,units="secs"))),"seconds\n")
    if(interactive()&&requireNamespace("beepr",quietly=TRUE))try(beepr::beep(),silent=TRUE)},add=TRUE)
  base<-"EuropeanFootball/pipeline_data/Manual_Sources/Season_2025_26"
  out<-file.path(base,"club_article_evidence");cache<-file.path(out,"cache")
  dir.create(cache,recursive=TRUE,showWarnings=FALSE)
  rows<-fread(file.path(base,"material_hold_review/historical_identity_candidates.csv"),encoding="UTF-8")
  rows<-unique(rows,by=c("Country","WikiName"))
  m<-fread("EuropeanFootball/pipeline_data/Matches_Clean_Combined/european_football_all_matches.csv",
    select=c("Country","Home","Away","CompetitionType"),encoding="UTF-8")
  known<-unique(rbind(m[CompetitionType=="league",.(Country,Name=Home)],m[CompetitionType=="league",.(Country,Name=Away)]))
  normal<-function(x)gsub("[^a-z0-9]","",tolower(stri_trans_general(x,"Latin-ASCII")))
  saved<-list();blocked<-FALSE
  cat("Checking linked articles for",nrow(rows),"club names; 10-second download pauses.\n")
  for(i in seq_len(nrow(rows))) {
    r<-rows[i];country<-r$Country
    cat(sprintf("  [%d/%d] %s: %s\n",i,nrow(rows),country,r$WikiName))
    warnings_seen<-character();result<-data.table(r,ArticleURL="",LeadHistoricalNames="",EvidenceDecision="ARTICLE_SCOPE_REVIEW",Error="")
    tryCatch({
      season_path<-file.path(base,"score_date_matching/cache",paste0(gsub("[^A-Za-z0-9]","_",country),".html"))
      doc<-read_html(season_path)
      links<-xml_find_all(doc,"//table//a[@href]")
      # Only an exact season-table label is used; no search ranking or fuzzy link.
      target<-normal(r$WikiName)
      hrefs<-xml_attr(links[normal(xml_text(links))==target],"href")
      hrefs<-hrefs[!grepl("#|File:|Category:|action=",hrefs)]
      hrefs<-hrefs[grepl("^/wiki/|^[.]/|^https://en.wikipedia.org/wiki/",hrefs)]
      hrefs<-sub("^https://en.wikipedia.org/wiki/|^/wiki/|^[.]/","",hrefs)
      hrefs<-unique(vapply(hrefs,URLdecode,character(1)))
      if(length(hrefs)!=1L)stop("Season-table club link is absent or ambiguous")
      href<-hrefs[1]
      url<-paste0("https://en.wikipedia.org/wiki/",URLencode(href,reserved=TRUE))
      set(result,j="ArticleURL",value=url)
      path<-file.path(cache,paste0(gsub("[^A-Za-z0-9]","_",paste(country,r$WikiName,sep="_")),".html"))
      if(!file.exists(path)) {
        if(Sys.getenv("CLUB_ARTICLE_CACHE_ONLY","0")=="1")stop("Article not cached; offline run holds this club")
        Sys.sleep(10);old<-options(timeout=40)
        tryCatch(withCallingHandlers(download.file(url,paste0(path,".part"),mode="wb",quiet=TRUE,method="libcurl",
          headers=c("User-Agent"="J-Ratings-Season-Audit/1.0 (local football data research)")),
          warning=function(w){warnings_seen<<-c(warnings_seen,conditionMessage(w));invokeRestart("muffleWarning")}),finally=options(old))
        page<-read_html(paste0(path,".part"))
        if(is.na(xml_text(xml_find_first(page,"//h1"))))stop("No club article heading")
        if(!file.rename(paste0(path,".part"),path))stop("Cannot save article cache")
      }
      page<-read_html(path)
      # Lead paragraphs and infobox bold names only. Full article text would
      # include opponents and other clubs and cannot establish identity.
      lead_nodes<-xml_find_all(page,"//*[contains(concat(' ',normalize-space(@class),' '),' mw-parser-output ')]/p[not(preceding-sibling::h2) and not(preceding-sibling::div[contains(@class,'mw-heading')])]")
      sections<-xml_find_all(page,"//section[@data-mw-section-id='0']")
      if(length(sections))lead_nodes<-xml_find_all(sections[[1]],"./p")
      lead<-paste(xml_text(head(lead_nodes,4)),collapse=" ")
      bold<-xml_find_all(page,"//*[contains(concat(' ',normalize-space(@class),' '),' mw-parser-output ')]/p[not(preceding-sibling::h2) and not(preceding-sibling::div[contains(@class,'mw-heading')])]//b | //table[contains(@class,'infobox')]//th[contains(@class,'fn')]")
      if(length(sections))bold<-xml_find_all(sections[[1]],"./p//b | .//table[contains(@class,'infobox')]//th[contains(@class,'fn')]")
      if(!length(lead_nodes)||!nzchar(trimws(lead)))stop("Lead extraction empty; no identity conclusion")
      subjects<-unique(normal(xml_text(bold)));subjects<-subjects[nchar(subjects)>=5L]
      candidates<-known[Country==country]
      hits<-unique(candidates[normal(Name) %chin% subjects,Name])
      set(result,j="LeadHistoricalNames",value=paste(hits,collapse="; "))
      set(result,j="EvidenceDecision",value=if(length(hits)==1L)"EXACT_LEAD_SUBJECT_NAME_EVIDENCE_REVIEW"else if(length(hits)>1L)"MULTIPLE_EXISTING_SUBJECT_NAMES_REVIEW"else "NO_EXACT_EXISTING_SUBJECT_NAME")
      # Save short evidence excerpt, not an automatic alias instruction.
      set(result,j="LeadExcerpt",value=substr(gsub("[[:space:]\u00a0]+"," ",lead),1,800))
    },error=function(e){
      detail<-paste(c(conditionMessage(e),warnings_seen),collapse=" | ")
      set(result,j="Error",value=detail);cat("    Held:",detail,"\n")
      if(grepl("403|429",detail))blocked<<-TRUE
    })
    saved[[i]]<-result;fwrite(rbindlist(saved,fill=TRUE),file.path(out,"club_article_review.csv"))
    if(blocked){cat("Access restriction: requests stopped; cached progress retained.\n");break}
  }
  print(rbindlist(saved,fill=TRUE)[,.N,by=EvidenceDecision])
  cat("No aliases or games changed. Report:",normalizePath(out,winslash="/"),"\n")
}
check_target_club_articles()
