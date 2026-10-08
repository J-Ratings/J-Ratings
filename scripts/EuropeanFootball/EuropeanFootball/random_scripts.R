# Bulgaria RSSSF experiment: source from the J-Ratings root.
# Scope: men's national top flight/championship, not cups or lower divisions.
# The existing master is NEVER an output of this script. Downloads are cached.
library(xml2)
library(data.table)
ROOT <- normalizePath(Sys.getenv("J_RATINGS_REPO", getwd()), winslash = "/")
stopifnot(dir.exists(file.path(ROOT, "EuropeanFootball")))
CACHE <- file.path(ROOT, "EuropeanFootball/pipeline_data/Source/rsssf/bulgaria/archive")
OUT <- file.path(ROOT, "EuropeanFootball/pipeline_data/Manual_Sources/Bulgaria")
NEW_MASTER <- file.path(ROOT, "EuropeanFootball/pipeline_data/Matches_Clean_Combined/european_football_master_v2.csv")
OLD_MASTER <- file.path(ROOT, "EuropeanFootball/pipeline_data/Matches_Clean_Combined/european_football_all_matches.csv")
OLD_CHECKSUM <- tools::md5sum(OLD_MASTER)
stopifnot(NEW_MASTER != OLD_MASTER)
dir.create(CACHE, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
options(timeout = 90)
fetch <- function(url) {
  path <- file.path(CACHE, basename(url))
  if (!file.exists(path)) {
    Sys.sleep(1)
    tmp <- paste0(path, ".tmp")
    on.exit(unlink(tmp))
    message("Downloading ", url)
    status <- download.file(url, tmp, method = "libcurl", mode = "wb", quiet = TRUE)
    if (status != 0L) stop("Download failed: ", url)
    doc <- read_html(tmp)
    if (!length(xml_find_all(doc, "//pre | //a"))) stop("Unexpected response: ", url)
    if (!file.rename(tmp, path)) stop("Could not save ", path)
  }
  path
}
links <- function(path, url) {
  a <- xml_find_all(read_html(path), "//a[@href]")
  data.table(Label = trimws(xml_text(a)), URL = url_absolute(xml_attr(a, "href"), url))
}
index_urls <- paste0("https://www.rsssf.org/", c("resultsp.html", "resultsp99.html",
 "resultsp00.html", "resultsp2010.html", "resultsp2020.html", "tablesb/bulghist.html"))
inventory <- rbindlist(lapply(index_urls, function(u) links(fetch(u), u)))
inventory <- unique(inventory[grepl("^https://www.rsssf.org/tablesb/bulg[0-9]+[.]html$", URL)], by = "URL")
# Verified newer page not yet listed in the completed-season index.
inventory <- unique(rbind(inventory, data.table(Label="Bulgaria 2025/26", URL="https://www.rsssf.org/tablesb/bulg2026.html")), by="URL")
download_log <- rbindlist(lapply(inventory$URL, function(u) {
  tryCatch({
    p <- fetch(u)
    data.table(URL = u, File = p, Status = "downloaded_or_cached",
      Title = xml_text(xml_find_first(read_html(p), "//title")), Error = "")
  }, error = function(e) data.table(URL = u, File = "", Status = "download_failed",
                                  Title = "", Error = conditionMessage(e)))
}), fill = TRUE)
download_log[, MD5 := vapply(File,function(p) if(file.exists(p)) unname(tools::md5sum(p)) else "",character(1))]
download_log[, CachedUTC := vapply(File,function(p) if(file.exists(p)) format(file.info(p)$mtime,"%Y-%m-%dT%H:%M:%SZ",tz="UTC") else "",character(1))]
fwrite(download_log, file.path(OUT, "download_audit.csv"))
message("Archive pages available: ", sum(download_log$Status == "downloaded_or_cached"))



# Extract only the first national competition section. Historic regional leagues,
# domestic cups and lower tiers are deliberately outside this experiment.
top_text <- function(doc) {
  nodes <- xml_find_all(doc, "//h2 | //h3 | //h4 | //pre")
  pieces <- character()
  begun <- FALSE
  for (node in nodes) {
    tag <- xml_name(node)
    value <- xml_text(node)
    if (tag == "h2") next
    if (tag %in% c("h3", "h4")) {
      if (begun) break
      if (!grepl("State Championship|State Division|A.? RFG|A.*grupa|Prva", value, ignore.case=TRUE)) return("")
      begun <- TRUE
    } else {
      begun <- TRUE
      pieces <- c(pieces, value)
    }
  }
  x <- unlist(strsplit(paste(pieces, collapse="\n"), "\n", fixed=TRUE))
  # Older pages put all competitions in a single pre block.
  boundary <- grep("^(Bulgaria[n]? )?(Cup|Kupa|Super.?Cup)|^League Levels:|^B grupa|^B Group|^Second (Division|Level)|^Vtora", trimws(x), ignore.case=TRUE)
  if (length(boundary)) x <- head(x, boundary[1]-1L)
  paste(x, collapse="\n")
}
capture <- function(pattern, x) {
  m <- regexec(pattern, x, perl=TRUE, ignore.case=TRUE)
  z <- regmatches(x, m)[[1]]
  if (length(z)) z[-1] else character()
}
clean <- function(x) trimws(gsub("[[:space:]]+", " ", gsub("\u00a0", " ", x, fixed=TRUE)))
date_value <- function(token, season) {
  # Ambiguous date ranges are retained as missing, never silently midpointed.
  token <- sub(",\\s*([0-9]{1,2}:|[[:alpha:]]).*$", "", token, perl=TRUE)
  token <- trimws(gsub("[,]", " ", token))
  if (grepl("[0-9]\\s*[-/]\\s*[0-9]", token)) return(NA_character_)
  z <- capture("^(Jan[a-z]*|Feb[a-z]*|Mar[a-z]*|Apr[a-z]*|May|Jun[a-z]*|Jul[a-z]*|Aug[a-z]*|Sep[a-z]*|Oct[a-z]*|Nov[a-z]*|Dec[a-z]*)\\s+([0-9]{1,2})(?:\\s+([0-9]{4}))?\\s*$", token)
  if (length(z)) {
    mo <- match(tolower(substr(z[1],1,3)), tolower(month.abb)); day <- as.integer(z[2]); yr <- z[3]
  } else {
    z <- capture("^([0-9]{1,2})\\s+(Jan[a-z]*|Feb[a-z]*|Mar[a-z]*|Apr[a-z]*|May|Jun[a-z]*|Jul[a-z]*|Aug[a-z]*|Sep[a-z]*|Oct[a-z]*|Nov[a-z]*|Dec[a-z]*)(?:\\s+([0-9]{4}))?\\s*$", token)
    if (!length(z)) return(NA_character_)
    day <- as.integer(z[1]); mo <- match(tolower(substr(z[2],1,3)),tolower(month.abb)); yr <- z[3]
  }
  first <- as.integer(substr(season,1,4))
  # For split seasons, July-Dec belongs to the starting year. Explicit years win.
  if (is.na(yr) || !nzchar(yr)) yr <- first + as.integer(grepl("/",season) && mo < 7)
  candidate <- sprintf("%04d-%02d-%02d",as.integer(yr),mo,day)
  d <- suppressWarnings(as.Date(candidate, format="%Y-%m-%d"))
  if (is.na(d)) NA_character_ else as.character(d)
}
parse_season <- function(txt, season, url, path) {
  lines <- unlist(strsplit(txt,"\n",fixed=TRUE))
  rows <- list(); rejected <- list()
  active_date <- NA_character_; date_label <- ""; stage <- "Regular season"; round <- ""
  for (i in seq_along(lines)) {
    raw <- trimws(lines[i])
    if (!nzchar(raw)) next
    # Tables have several numerical fields before the goals total.
    if (grepl("^\\s*[0-9]+\\s*[.]?\\s*.*\\s+[0-9]+\\s+[0-9]+\\s+[0-9]+\\s+[0-9]+\\s+[0-9]+\\s*[-:]",raw,perl=TRUE)) next
    if (grepl("^(Round|Semifinal|Quarterfinal|Final\\b|.*Leg\\b|Replay\\b)",raw,ignore.case=TRUE)) {
      active_date <- NA_character_; date_label <- ""
      round <- sub("\\s*\\[.*","",raw)
    }
    if (grepl("^(Preliminary Stage|Second Stage|First Group|Second Group|Third Group|Championship (Group|Playoff)|Relegation (Group|Playoff)|Group [AB]|Regular Stage|Playoff Stage|.*League Playoff|.*Promotion/Relegation Playoffs?)\\s*$",raw,ignore.case=TRUE)) {
      stage <- raw; round <- ""; active_date <- NA_character_; date_label <- ""
    }
    # Standard RSSSF score-in-the-middle format.
    z <- capture("^([[:alpha:]][^<>\\[\\]]*?)\\s+([0-9]{1,2})\\s*[-:]\\s*([0-9]{1,2})\\s+(.+)$",raw)
    # Older format: home - away followed by score.
    old <- capture("^([[:alpha:]].+?)\\s+-\\s+(.+?)\\s+([0-9]{1,2})\\s*[-:]\\s*([0-9]{1,2})(.*)$",raw)
    if (length(old)) z <- c(old[1],old[3],old[4],old[2])
    brackets <- regmatches(raw, gregexpr("\\[[^]]+\\]",raw,perl=TRUE))[[1]]
    date_tokens <- gsub("^\\[|\\]$","",brackets)
    month_pattern <- "(Jan(uary)?|Feb(ruary)?|Mar(ch)?|Apr(il)?|May|Jun(e)?|Jul(y)?|Aug(ust)?|Sep(tember)?|Oct(ober)?|Nov(ember)?|Dec(ember)?)"
    date_tokens <- date_tokens[grepl(paste0("^(",month_pattern,"\\s+[0-9]|[0-9]{1,2}\\s+",month_pattern,"\\b)"),date_tokens,ignore.case=TRUE,perl=TRUE)]
    if (!length(z)) {
      if (length(date_tokens)) {
        date_label <- tail(date_tokens,1); active_date <- date_value(date_label,season)
      } else if (grepl("Table|^Cross",raw,ignore.case=TRUE)) {
        active_date <- NA_character_; date_label <- ""
      }
      if (grepl("[[:alpha:]].*\\s([0-9]+[-:][0-9]+|awd|w/o|n/p|ppd|abd)\\s",raw,perl=TRUE) &&
          !grepl("^(NB|\\[|[0-9]|[-=]|Round|Final|For |The |In |All |all )",raw)) {
        rejected[[length(rejected)+1L]] <- data.table(Season=season,SourcePage=url,SourceLine=i,RawLine=raw)
      }
      next
    }
    if (grepl("^(NB:|Round |Final |Note|Replay:|Total|Cross|Attendance|Aggregate)",z[1],ignore.case=TRUE)) next
    if (grepl("[,;]|[0-9]+(og|pen)|\\b(ann|awd|originally)\\b",z[1],ignore.case=TRUE,perl=TRUE)) next
    home <- clean(z[1]); away_full <- z[4]
    # A score grid row is not one fixture (handled separately below).
    if (length(regmatches(raw,gregexpr("[0-9]+[-:a][0-9]+",raw,perl=TRUE))[[1]]) > 3L) next
    away <- sub("\\s*\\[.*$","",away_full)
    away <- sub("\\s{2,}.*$","",away)
    away <- clean(sub("\\s+\\([0-9]+[-:][0-9]+\\).*$","",away))
    notes <- paste(brackets,collapse=" ")
    reason <- character()
    if(length(old) && grepl("[0-9]+[-:][0-9]+",gsub("\\([0-9]+[-:][0-9]+\\)","",old[5]))) reason <- c(reason,"multiple_scores_need_review")
    if (grepl("\\b(awd|awarded|wo|w/o|abd|abandoned|annulled|aet|pen)\\b",raw,ignore.case=TRUE)) reason <- c(reason,"special_result")
    if (grepl("Promotion|Relegation Playoff",stage,ignore.case=TRUE) && !grepl("Group|Round",round,ignore.case=TRUE)) reason <- c(reason,"promotion_playoff_scope_review")
    # Conservative: do not turn trailing outcome annotations into team names.
    if (grepl("\\b(awd|awarded|wo|aet|pen)\\b",away,ignore.case=TRUE)) {
      away <- clean(sub("\\s+(awd|awarded|wo|aet|pen)\\b.*","",away,ignore.case=TRUE))
    }
    when <- active_date; label <- date_label
    if (length(date_tokens)) { label <- tail(date_tokens,1); when <- date_value(label,season) }
    if (is.na(when)) reason <- c(reason,if (nzchar(label)) "ambiguous_date" else "missing_date")
    # Early split seasons sometimes ran into autumn of the ending year; require
    # explicit years there rather than applying the modern July boundary.
    if (as.integer(substr(season,1,4)) < 1940 && nzchar(label) && !grepl("[0-9]{4}",label)) reason <- c(reason,"historic_year_unverified")
    if (!grepl("[[:alpha:]]",away) || home == away) reason <- c(reason,"invalid_team")
    rows[[length(rows)+1L]] <- data.table(
      Season=season,Country="Bulgaria",Competition="bulgaria_top_flight",
      CompetitionType=if (as.integer(substr(season,1,4))<1937) "championship" else "league",
      Tier=1L,League=if(as.integer(substr(season,1,4))<1937) "Bulgarian State Championship" else "Bulgarian First League",Date=when,Home=home,Away=away,
      Result=if(as.integer(z[2])>as.integer(z[3])) "1-0" else if(as.integer(z[2])<as.integer(z[3])) "0-1" else "0.5-0.5",
      Score=paste(z[2],z[3],sep="-"),Source="rsssf",SourcePage=url,Stage=stage,
      DateApprox=NA,SourceFile=path,Round=round,SourceLine=i,SourceDate=label,
      SourceHome=home,SourceAway=away,RawLine=raw,
      ReviewReason=paste(unique(reason),collapse=";"))
  }
  list(matches=rbindlist(rows,fill=TRUE),unparsed=rbindlist(rejected,fill=TRUE))
}

# Score grids are useful historical results, but never enter the dated master.
parse_grid <- function(txt, season, url, path) {
  lines <- unlist(strsplit(txt,"\n",fixed=TRUE))
  ix <- which(vapply(lines,function(x)
    length(regmatches(x,gregexpr("[0-9]{1,2}[-:a][0-9]{1,2}",x,perl=TRUE))[[1]])>=5L,logical(1)))
  if(!length(ix)) return(data.table())
  # Restrict to named rows with >=5 cells, excluding numeric tables and prose.
  ix <- ix[grepl("^\\s*[[:alpha:]]",lines[ix])]
  if(!length(ix)) return(data.table())
  groups <- split(ix,cumsum(c(TRUE,diff(ix)>1L)))
  results <- list()
  for(g in groups) {
    if(length(g)<5L) next
    names <- trimws(sub("\\s+(xxx|[0-9]{1,2}[-:a][0-9]{1,2}).*$","",lines[g],perl=TRUE))
    n <- length(g)
    cells <- lapply(lines[g],function(x)
      regmatches(x,gregexpr("xxx|[0-9]{1,2}[-:a][0-9]{1,2}",x,perl=TRUE))[[1]])
    if(!all(lengths(cells) %in% c(n,n-1L))) next
    for(i in seq_len(n)) {
      scores <- cells[[i]]
      if(length(scores)==n-1L) scores <- append(scores,"xxx",after=i-1L)
      if(scores[i]!="xxx") next
      for(j in setdiff(seq_len(n),i)) {
        s <- scores[j]; z <- strsplit(s,"[-:a]")[[1]]
        if(length(z)!=2L) next
        results[[length(results)+1L]] <- data.table(Season=season,Country="Bulgaria",
          Competition="bulgaria_top_flight",CompetitionType="league",Tier=1L,
          League="Bulgarian First League",Date=NA_character_,Home=names[i],Away=names[j],
          Result=if(as.integer(z[1])>as.integer(z[2]))"1-0" else if(as.integer(z[1])<as.integer(z[2]))"0-1" else "0.5-0.5",
          Score=paste(z,collapse="-"),Source="rsssf",SourcePage=url,
          Stage="Regular season",DateApprox=NA,SourceFile=path,Round="",
          SourceLine=g[i],SourceDate="",SourceHome=names[i],SourceAway=names[j],
          RawLine=trimws(lines[g[i]]),
          ReviewReason=if(grepl("a",s)) "missing_date;score_grid;special_result" else "missing_date;score_grid")
      }
    }
  }
  rbindlist(results,fill=TRUE)
}
first_table <- function(txt) {
  lines <- unlist(strsplit(txt,"\n",fixed=TRUE))
  rows <- list()
  for(line in lines) {
    z <- capture("^\\s*([0-9]+)\\s*\\.\\s*(.+?)\\s+([0-9]+)\\s+([0-9]+)\\s+([0-9]+)\\s+([0-9]+)\\s+([0-9]+)\\s*[-:]\\s*([0-9]+)\\s+",line)
    if(!length(z)) next
    rank <- as.integer(z[1])
    if(rank==1L && length(rows)) break
    if(!length(rows) && rank!=1L) next
    rows[[length(rows)+1L]] <- data.table(Team=clean(z[2]),P=as.integer(z[3]),
                   W=as.integer(z[4]),D=as.integer(z[5]),L=as.integer(z[6]),
                   GF=as.integer(z[7]),GA=as.integer(z[8]))
  }
  rbindlist(rows)
}
season_check <- function(txt,m) {
  tab <- first_table(txt)
  if(nrow(tab)<4L) return(list(ExpectedRegular=NA_real_,ParsedRegular=0L,
                    RegularCheck="no_comparable_table"))
  reg <- if(nrow(m)) m[Stage %in% c("Regular season","Regular Stage","Preliminary Stage")] else data.table()
  expected <- sum(tab$P)/2
  if(!nrow(reg)) return(list(ExpectedRegular=expected,ParsedRegular=0L,RegularCheck="no_regular_results"))
  # Avoid double-counting same source fixture repeated in a results list.
  reg <- unique(reg,by=c("Home","Away","Score","Round"))
  total_goals <- sum(as.integer(unlist(strsplit(reg$Score,"-",fixed=TRUE))))
  stats <- rbind(
    reg[,.(Team=Home,GF=as.integer(sub("-.*","",Score)),GA=as.integer(sub(".*-","",Score)))],
    reg[,.(Team=Away,GF=as.integer(sub(".*-","",Score)),GA=as.integer(sub("-.*","",Score)))])
  stats <- stats[,.(P=.N,W=sum(GF>GA),D=sum(GF==GA),L=sum(GF<GA),GF=sum(GF),GA=sum(GA)),by=Team]
  signature <- function(d) sort(do.call(paste,c(d[,.(P,W,D,L,GF,GA)],sep="|")))
  pass <- identical(signature(stats),signature(tab))
  list(ExpectedRegular=expected,ParsedRegular=nrow(reg),
       RegularCheck=if(pass) "all_team_statistics_match" else
         if(nrow(reg)==expected && total_goals==sum(tab$GF)) "count_and_goals_match_only" else "mismatch_or_partial")
}

# Parse detailed pages and use the historical overview only for seasons without
# a dedicated page. This makes table-only historical seasons visible in the audit.
jobs <- list()
# Regression checks for formats encountered in the actual archive.
stopifnot(identical(date_value("Aug 11, 18:00","2007/08"),"2007-08-11"),
          identical(date_value("May 29, Razgrad","2024/25"),"2025-05-29"),
          is.na(date_value("Aug 14-16","1998/99")))
probe <- parse_season(paste("Round 1 [Aug 8]", "Alpha 1-0 Beta",
  "  [Marcio Silva 48]", "Gamma 0-0 Delta", "Epsilon 2-1 Zeta [Sep 18]",
  "Eta 1-1 Theta", "Round 2", "Alpha 0-0 Gamma",sep="\n"),"2024/25","test","test")$matches
stopifnot(identical(probe$Date,c("2024-08-08","2024-08-08","2024-09-18","2024-08-08",NA_character_)))
probe_old <- parse_season("Round 1 [Aug 8]\nAlpha (City) - Beta (Town) 2:1 (1:1)","1996/97","test","test")$matches
stopifnot(probe_old$Away=="Beta (Town)",probe_old$Score=="2-1")
stopifnot(!grepl("Cupteam",top_text(read_html("<html><body><pre>Alpha 1-0 Beta\nCup Final\nCupteam 2-0 Other</pre></body></html>"))))
for (i in which(download_log$Status=="downloaded_or_cached")) {
  row <- download_log[i]
  season <- capture("Bulgaria\\s+([0-9]{4}(?:/[0-9]{2})?)",row$Title)
  if (!length(season)) next
  jobs[[season]] <- list(text=top_text(read_html(row$File)), url=row$URL, path=row$File)
}
history_path <- file.path(CACHE,"bulghist.html")
history_doc <- read_html(history_path)
history_nodes <- xml_find_all(history_doc,"//h2 | //pre")
hs <- NULL
for (node in history_nodes) {
  value <- xml_text(node)
  if (xml_name(node)=="h2") {
    ss <- capture("^\\s*([0-9]{4}(?:/[0-9]{2})?)\\s*$",value)
    hs <- if(length(ss)) ss else NULL
  } else if (!is.null(hs) && is.null(jobs[[hs]])) {
    jobs[[hs]] <- list(text=value,url="https://www.rsssf.org/tablesb/bulghist.html",path=history_path)
  }
}
all_rows <- list(); audits <- list(); unparsed <- list()
for (season in sort(names(jobs))) {
  job <- jobs[[season]]
  parsed <- parse_season(job$text,season,job$url,job$path)
  m <- rbindlist(list(parsed$matches,parse_grid(job$text,season,job$url,job$path)),fill=TRUE)
  check <- season_check(job$text,m)
  all_rows[[season]] <- m
  unparsed[[season]] <- parsed$unparsed
  audits[[season]] <- data.table(Season=season,SourcePage=job$url,
    ExpectedRegular=check$ExpectedRegular,ParsedRegular=check$ParsedRegular,RegularCheck=check$RegularCheck,
    Extracted=nrow(m),Dated=if(nrow(m)) sum(!is.na(m$Date)) else 0L,
    Ready=if(nrow(m)) sum(m$ReviewReason=="") else 0L,
    Review=if(nrow(m)) sum(m$ReviewReason!="") else 0L,
    UnparsedLines=nrow(parsed$unparsed),
    Status=if(!nzchar(job$text)) "outside_national_scope" else if(!nrow(m)) "no_match_results_extracted" else "parsed_requires_season_audit")
}
matches <- rbindlist(all_rows,fill=TRUE)
stopifnot(nrow(matches)>0)
audit <- rbindlist(audits)
# Conflicting same-day fixture scores are held for review; identical records
# repeated in a page are collapsed. Round/stage are metadata, not fixture identity.
matches <- rbindlist(list(unique(matches[!is.na(Date)],by=c("Season","Date","Home","Away","Score")),
                         unique(matches[is.na(Date)],by=c("Season","Home","Away","Score","SourceDate","Round","Stage"))))
matches[, Conflict := !is.na(Date) & uniqueN(Score)>1L,by=.(Season,Date,Home,Away)]
matches[Conflict==TRUE, ReviewReason:=paste0(ReviewReason,";conflicting_score")]
matches[,Conflict:=NULL]
# Recompute audit totals after deduplication and conflict checks.
for(s in audit$Season) {
  mm <- matches[Season==s]
  audit[Season==s, `:=`(Extracted=nrow(mm),Dated=sum(!is.na(mm$Date)),
                        Ready=sum(mm$ReviewReason==""),Review=sum(mm$ReviewReason!=""))]
}
matches[audit,on="Season",RegularSeasonCheck:=i.RegularCheck]
tables <- rbindlist(lapply(names(jobs),function(s) {
  t <- first_table(jobs[[s]]$text)
  if(nrow(t)) t[,`:=`(Season=s,Source="rsssf",SourcePage=jobs[[s]]$url)]
  t
}),fill=TRUE)
fwrite(tables,file.path(OUT,"historical_regular_tables.csv"))
fwrite(matches[ReviewReason!=""],file.path(OUT,"bulgaria_matches_needing_review.csv"),na="")
fwrite(rbindlist(unparsed,fill=TRUE),file.path(OUT,"unparsed_lines.csv"),na="")
fwrite(audit,file.path(OUT,"season_audit.csv"))
ready <- matches[ReviewReason==""]
ready[,DateApprox:=FALSE]
ready[,ValidationStatus:="dated_result_extracted;team_names_not_canonicalised"]
stopifnot(!anyNA(as.Date(ready$Date)),all(grepl("^[0-9]+-[0-9]+$",ready$Score)),
          !any(grepl("[0-9]+[-:][0-9]+|\\[|\\]",paste(ready$Home,ready$Away))),
          !anyDuplicated(ready,by=c("Season","Date","Home","Away")))
# Verified full regular-season statistics, including all 240 games in 2024/25.
stopifnot(audit[Season=="2024/25",RegularCheck]=="all_team_statistics_match",
          matches[Season=="2024/25" & Stage=="Regular Stage",.N]==240L)
# This is a new-data-only master. Keep other countries if added on future runs.
if (file.exists(NEW_MASTER)) {
  previous <- fread(NEW_MASTER, colClasses="character")
  if (!all(c("Country","Source") %in% names(previous))) stop("Unexpected new master schema.")
  previous <- previous[!(Country=="Bulgaria" & Source=="rsssf")]
  if(nrow(previous)) ready <- rbindlist(list(previous,ready),use.names=TRUE,fill=TRUE)
}
setorder(ready,Date,Country,Competition,Home,Away)
master_tmp <- paste0(NEW_MASTER,".tmp")
fwrite(ready,master_tmp,na="")
stopifnot(nrow(fread(master_tmp))==nrow(ready))
if(!file.copy(master_tmp,NEW_MASTER,overwrite=TRUE)) stop("Could not save new master")
unlink(master_tmp)
fwrite(unique(matches[,.(SourceHome,SourceAway)]),file.path(OUT,"source_team_pairs.csv"))
message("New master: ",NEW_MASTER,"\nRows: ",nrow(ready),
        "\nReview rows: ",sum(matches$ReviewReason!=""))
stopifnot(identical(OLD_CHECKSUM,tools::md5sum(OLD_MASTER)))
writeLines(c(
  "# Bulgaria RSSSF import experiment",
  paste("Generated:",format(Sys.time(),tz="UTC")),
  "Scope: men's national top flight and historical national championship; cups and regional/lower leagues excluded.",
  "The new master contains new Bulgaria data only. The existing master and Elo pipeline are unchanged.",
  paste("Downloaded season pages:",sum(download_log$Status=="downloaded_or_cached")),
  paste("Seasons audited (including out-of-scope regional predecessors):",nrow(audit)),
  paste("Dated rows in new master:",sum(ready$Country=="Bulgaria" & ready$Source=="rsssf")),
  paste("Rows held for review:",sum(matches$ReviewReason!="")),
  paste("Regular-season full team-statistics checks passed:",sum(audit$RegularCheck=="all_team_statistics_match")),
  "",
  "This is an experimental dataset, not a claim of complete historical coverage or production-ready Elo input.",
  "Source names are preserved; team aliases across years and against UEFA matches still require harmonisation.",
  "RegularSeasonCheck compares regular-stage P/W/D/L/GF/GA against the first published table, independent of spelling.",
  "A passing regular-season check does not validate all playoff results or prove every date is correct.",
  "Mismatches can reflect incomplete coverage, awarded/annulled games, source errors or unresolved names.",
  "No invented dates: score grids, undated rounds, date ranges, special results and ambiguous records go to review.",
  "Annulled/awarded/replayed fixtures mentioned only in explanatory prose may require additional manual review.",
  "SourcePage, SourceFile, SourceLine, SourceDate and RawLine allow inspection of every imported result.",
  "SourceLine is the line within extracted national-section text, not the original HTML file.",
  "historical_regular_tables.csv contains the first regular table, including seasons with no match list.",
  "season_audit.csv reports extraction and coverage; unparsed_lines.csv retains nonstandard result lines.",
  "download_audit.csv records page hashes and cache timestamps. Original HTML retains RSSSF author credits.",
  "Rerun scripts/EuropeanFootball/random_scripts.R from the repo root; cached pages avoid new downloads.",
  "Future reruns replace RSSSF Bulgaria rows in v2 and retain other countries/sources already added there.",
  paste("Original master MD5 (unchanged):",unname(OLD_CHECKSUM))
),file.path(OUT,"README.md"))
