# Offline pilot: cached Wikipedia score matrices versus a dated results archive.
# Never changes aliases, ratings or the production master.
suppressPackageStartupMessages({library(data.table); library(xml2); library(rvest)})
out <- "EuropeanFootball/pipeline_data/Manual_Sources/Season_2025_26/Wales_test"
dir.create(out, recursive=TRUE, showWarnings=FALSE)
cat("[1/3] Reading cached Wikipedia results...\n")
# Load only the existing parser functions, without executing its full pipeline.
needed <- c("clean_text","clean_team_name","team_key","capture","empty_games",
            "wiki_games","wiki_link_identities")
for (expr in parse("scripts/EuropeanFootball/wikipedia_rsssf_four_leagues.R")) {
  if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
      as.character(expr[[2]])[1] %in% needed) eval(expr)
}
w <- wiki_games(read_html(file.path(out,"sources/wiki.html")))$games
w[, StageKey := fifelse(grepl("Championship Conference",Stage),"championship",
                         fifelse(grepl("Play-Off Conference",Stage),"relegation",
                                 fifelse(grepl("Regular season",Stage,ignore.case=TRUE),"regular","playoff")))]
fwrite(w,file.path(out,"wikipedia_results.csv"))
cat("[2/3] Reading dated archive match records...\n")
doc <- read_html(file.path(out,"sources/dated.html"))
nodes <- xml_find_all(doc,"//a[contains(concat(' ',normalize-space(@class),' '),' gsa-match-rounds-v2__match ')]")
d <- rbindlist(lapply(nodes,function(n) {
  teams <- sub("^logo of ","",xml_attr(xml_find_all(n,".//img[starts-with(@alt,'logo of ')]"),"alt"))
  score <- gsub("[[:space:]\u00a0]+","",xml_text(xml_find_first(n,".//*[contains(@class,'__match-score')]")))
  url <- xml_attr(n,"href")
  if(length(teams)!=2L || !grepl("^[0-9]+:[0-9]+$",score)) return(NULL)
  data.table(Date=sub(".*/match/([0-9-]+)/.*","\\1",url),Home=teams[1],Away=teams[2],
             Score=sub(":","-",score,fixed=TRUE),SourceURL=url)
}),fill=TRUE)
if(!nrow(d)) stop("No dated archive records exposed; no matches have been changed.")
d <- unique(d,by="SourceURL")
# Explicit page labels; abbreviations are not resolved using fuzzy matching.
map <- c("Bala Town FC"="Bala Town","Barry Town United FC"="Barry Town United","Barry Town United AFC"="Barry Town United",
 "Briton Ferry Llansawel AFC"="Briton Ferry Llansawel","Caernarfon Town FC"="Caernarfon Town",
 "Cardiff Metropolitan University FC"="Cardiff Metropolitan University",
 "Colwyn Bay FC"="Colwyn Bay","Connah's Quay Nomads FC"="Connah's Quay Nomads",
 "Flint Town United FC"="Flint Town United","Haverfordwest County AFC"="Haverfordwest County",
 "Llanelli Town AFC"="Llanelli Town","Penybont FC"="Penybont","The New Saints FC"="The New Saints")
for(col in c("Home","Away")) {
  v <- map[d[[col]]]; v[is.na(v)] <- d[[col]][is.na(v)]; set(d,j=col,value=unname(v))
}
fwrite(d,file.path(out,"gsa_sample_results.csv"))
# The GSA landing page exposes selected rounds only. Use the complete fixture
# list for the main pilot, retaining GSA as an independent small cross-check.
doc2 <- read_html(file.path(out,"sources/transfermarkt.html"))
tm <- list()
for(tab in xml_find_all(doc2,"//table[.//a[contains(@class,'ergebnis-link')]]")) {
  date <- NA_character_
  heading <- xml_text(xml_find_first(tab,"preceding::div[contains(@class,'content-box-headline')][1]"))
  day <- as.integer(sub("^([0-9]+).*","\\1",heading))
  for(row in xml_find_all(tab,".//tbody/tr")) {
    date_link <- xml_attr(xml_find_first(row,".//a[contains(@href,'/datum/')]"),"href")
    if(!is.na(date_link)) date <- sub(".*/datum/","",date_link)
    sc <- xml_find_first(row,".//a[contains(@class,'ergebnis-link')]")
    score <- trimws(xml_text(sc))
    clubs <- unique(xml_attr(xml_find_all(row,".//a[contains(@href,'/spielplan/verein/')]"),"title"))
    if(is.na(score) || !grepl("^[0-9]+:[0-9]+$",score) || length(clubs)!=2L) next
    tm[[length(tm)+1L]] <- data.table(Date=date,Home=clubs[1],Away=clubs[2],Score=sub(":","-",score,fixed=TRUE),
      Matchday=day,SourceURL=paste0("https://www.transfermarkt.co.uk",xml_attr(sc,"href")))
  }
}
tm <- rbindlist(tm)
extra <- c("Penybont FC"="Penybont","Llanelli Town AFC"="Llanelli Town",
           "Cardiff Metropolitan University FC"="Cardiff Metropolitan University")
for(col in c("Home","Away")) {
  v <- extra[tm[[col]]]; v[is.na(v)] <- tm[[col]][is.na(v)]; set(tm,j=col,value=unname(v))
}
champ <- unique(w[StageKey=="championship",Home])
tm[, StageKey:=fifelse(Matchday<=22L,"regular",fifelse(Home %in% champ & Away %in% champ,"championship","relegation"))]
fwrite(tm,file.path(out,"dated_results.csv"))
cat("[3/3] Comparing unique stage/home/away evidence and scores...\n")
d <- tm
for(x in list(w,d)) x[, Key:=paste(StageKey,Home,Away,sep="|")]
counts <- w[,. (N=.N,WikiScore=paste(unique(Score),collapse=";")),by=Key]
d <- merge(d,counts,by="Key",all.x=TRUE)
d[, Decision:=fifelse(is.na(N),"NO_WIKIPEDIA_FIXTURE_MATCH",fifelse(N!=1L,"AMBIGUOUS_FIXTURE",fifelse(Score==WikiScore,"MATCHED_DATE_AND_SCORE","SCORE_DISAGREEMENT")))]
fwrite(d,file.path(out,"match_review.csv"))
fwrite(w[!Key %in% d$Key],file.path(out,"wikipedia_without_date.csv"))
parsed_dates <- as.Date(d$Date)
if(anyNA(parsed_dates) || any(parsed_dates<as.Date("2025-07-01") | parsed_dates>as.Date("2026-06-30"))) stop("Missing or out-of-season dates in pilot.")
if(anyDuplicated(d$Key)) stop("Duplicate stage fixtures in dated archive.")
appearances <- rbind(d[,.(Date,Team=Home)],d[,.(Date,Team=Away)])
conflicts <- appearances[,.N,by=.(Date,Team)][N>1L]
fwrite(conflicts,file.path(out,"club_date_conflicts.csv"))
if(nrow(conflicts)) stop("Club date conflicts require review.")
gsa <- fread(file.path(out,"gsa_sample_results.csv"))
cross <- merge(gsa,d[,.(Home,Away,Score,TransfermarktDate=Date)],by=c("Home","Away","Score"),all.x=TRUE,allow.cartesian=TRUE)
cross[,DateAgreement:=Date==TransfermarktDate]
fwrite(cross,file.path(out,"independent_date_check.csv"))
cat("Independent archive sample:",sum(cross$DateAgreement,na.rm=TRUE),"date agreements;",sum(!is.na(cross$TransfermarktDate)),"league comparisons;",sum(is.na(cross$TransfermarktDate)),"outside league list.\n")
fwrite(d[,.N,by=.(StageKey,Decision)],file.path(out,"summary.csv"))
writeLines(c("Wales 2025/26 pilot. Production master unchanged.",
 "Wikipedia: https://en.wikipedia.org/wiki/2025%E2%80%9326_Cymru_Premier",
 "Dates and independent scores: https://www.transfermarkt.co.uk/cymru-premier/gesamtspielplan/wettbewerb/WAL1/saison_id/2025",
 "Small independent date sample: https://globalsportsarchive.com/en/soccer/competition/jd-cymru-premier-2025-2026/76350",
 "Explicit club label mappings only; no fuzzy identities. Stages separated by matchday and Wikipedia championship roster.",
 "Wikipedia has 192 league matrix results plus one individual playoff report. The playoff is held separately.",
 "Agreement validates extraction and cross-source consistency, not proof that either source is error-free.",
 "Sources cached 2026-10-04. Re-running this script reads caches; it does not download or import."),file.path(out,"READ_ME.txt"))
print(w[,.N,by=StageKey]); print(d[,.N,by=Decision])
cat("Wikipedia results:",nrow(w),"; dated records exposed:",nrow(d),"\n")
cat("Report:",normalizePath(out,winslash="/"),"\nProduction master unchanged.\n")
if(interactive() && requireNamespace("beepr",quietly=TRUE)) beepr::beep()
