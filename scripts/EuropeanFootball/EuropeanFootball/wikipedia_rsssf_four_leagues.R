# Wikipedia fixtures/results + RSSSF dates. Run from the repository root:
# source("scripts/EuropeanFootball/wikipedia_rsssf_four_leagues.R")
# Optional: Sys.setenv(FOUR_LEAGUES_MIN_YEAR=2023, FOUR_LEAGUES_MAX_YEAR=2023)
# Years refer to the START year. Unset these to explore all available history.
# Requires data.table, xml2, rvest, stringi. Never writes the production master.
suppressPackageStartupMessages({library(data.table); library(xml2); library(rvest)})
if (!requireNamespace("stringi", quietly=TRUE)) stop("Install stringi first")
has_tictoc <- requireNamespace("tictoc", quietly=TRUE)

clean_text <- function(x) trimws(gsub("\\s+", " ", gsub("\\[[^]]*\\]", "", x)))
clean_team_name <- function(x) sub("(?i)\\s*\\([0-9]+(?:st|nd|rd|th)\\)\\s*$","",clean_text(x),perl=TRUE)
team_key <- function(x) {
  x <- tolower(stringi::stri_trans_general(clean_text(x), "Latin-ASCII"))
  # Standings positions on playoff participants are annotations, not identities.
  x <- gsub("\\s*\\([0-9]+(?:st|nd|rd|th)\\)\\s*$", "", x, perl=TRUE)
  x <- gsub("&", " and ", x, fixed=TRUE)
  x <- gsub("['’`.]", "", x)
  x <- gsub("[-_/()]", " ", x)
  x <- gsub("\\bthe\\b", " ", x)
  trimws(gsub(" +", " ", gsub("[^a-z0-9 ]", " ", x)))
}
capture <- function(pattern, x) regmatches(x, regexec(pattern, x, perl=TRUE))[[1]]
empty_games <- function() data.table(Home=character(), Away=character(), Score=character(), Stage=character())
phase_key <- function(x) {
  ifelse(grepl("(?i)relegation|play.out",x,perl=TRUE),"relegation",
    ifelse(grepl("(?i)championship.*(round|play|conference|group|phase|stage)|play.off round",x,perl=TRUE),"championship","regular"))
}

# Read result matrices by their home/away axis, not table position. Away names
# come from the corresponding row labels (headers are often abbreviations).
wiki_games <- function(doc) {
  out <- list(); audit <- list()
  # Footnote superscripts are not score digits (e.g. 0-3 followed by note 1).
  doc <- read_html(as.character(doc))
  xml_remove(xml_find_all(doc,"//table//sup"))
  # Some rendered Wikipedia templates emit rowspan='2style=...'. Repair the
  # numeric span before rvest expands the grid, rather than shifting team axes.
  for(cell in xml_find_all(doc,"//*[@rowspan or @colspan]")) for(attr in c("rowspan","colspan")) {
    value <- xml_attr(cell,attr)
    if(!is.na(value) && grepl("^[0-9]+",value)) xml_set_attr(cell,attr,sub("^([0-9]+).*$","\\1",value))
  }
  tables <- xml_find_all(doc, "//table")
  for (i in seq_along(tables)) {
    tab <- tables[[i]]
    # Layout tables may wrap multiple complete results matrices. Parse the
    # inner tables once; expanding the wrapper shifts axes and duplicates games.
    if(length(xml_find_all(tab,".//table"))) next
    d <- tryCatch(as.data.frame(html_table(tab, header=FALSE, trim=TRUE)), error=function(e) NULL)
    if (is.null(d) || ncol(d)<4L) next
    axis <- which(apply(d, 1, function(z) any(grepl("Home.*Away|Home.*away", z))))
    team_col <- 1L
    if (!length(axis)) {
      # Wikipedia sometimes embeds the result matrix to the right of standings.
      n <- nrow(d)-1L; h <- 1L
      if(n<4L || ncol(d)<n+3L || !any(grepl("^Team$",d[1,]))) next
      team_col <- which(grepl("^Team$",d[1,]))[1]
      score_cols <- seq.int(ncol(d)-n+1L,ncol(d))
      diag_cells <- vapply(seq_len(n),function(j) clean_text(as.character(d[j+1L,score_cols[j]])),character(1))
      if(any(!is.na(diag_cells) & !diag_cells %in% c("","\u2014","\u2013","-"))) next
    } else {
      h <- axis[1]
      block <- d[seq.int(h+1L,nrow(d)),,drop=FALSE]
      row_names <- clean_text(gsub("\\s*\\([CORPQ]+\\)$","",clean_text(as.character(block[[1]]))))
      # Drop explanatory footer rows, keeping rows containing a result or a
      # diagonal blank/dash. Repeated home rows are separate sets of fixtures.
      valid <- !is.na(row_names) & nzchar(row_names) & !grepl("^(Source:|Updated|Rules|Legend)|(?i)^Home.*Away",row_names,perl=TRUE)
      block <- block[valid,,drop=FALSE]; row_names <- row_names[valid]
      teams <- unique(row_names); n <- length(teams)
      if(n<2L || (ncol(d)-1L) %% n != 0L) next
      away_names <- rep(teams,(ncol(d)-1L)%/%n)
      heading <- clean_text(paste(vapply(c("h2","h3","h4"),function(tag)
        xml_text(xml_find_first(tab,paste0("preceding::",tag,"[1]"))),character(1)),collapse=" / "))
      scores <- clean_text(as.character(as.matrix(block[,-1,drop=FALSE])))
      home <- rep(row_names,times=length(away_names))
      away <- rep(away_names,each=nrow(block))
      valid_score <- !is.na(scores) & grepl("^[0-9]+\\s*[-\u2013\u2014:]\\s*[0-9]+$",scores) & home!=away
      parsed <- data.table(Home=home[valid_score],Away=away[valid_score],
        Score=gsub("\\s*[-\u2013\u2014:]\\s*","-",scores[valid_score]),Stage=heading,WikiTable=i)
      out[[length(out)+1L]] <- parsed
      audit[[length(audit)+1L]] <- data.table(Table=i,Stage=heading,Teams=n,
        PossibleCells=sum(home!=away),ParsedScores=nrow(parsed))
      next
    }
    if (nrow(d)<h+n) next
    block <- d[seq.int(h+1L,h+n), , drop=FALSE]
    teams <- clean_text(gsub("\\s*\\([CORPQ]+\\)$","",clean_text(as.character(block[[team_col]]))))
    if (any(!nzchar(teams)) || anyDuplicated(teams)) next
    heading <- paste(vapply(c("h2","h3","h4"),function(tag) xml_text(xml_find_first(tab,paste0("preceding::",tag,"[1]"))),character(1)),collapse=" / ")
    if (is.na(heading)) heading <- "Results"
    heading <- clean_text(heading)
    k <- 0L
    for (r in seq_len(n)) for (c in seq_len(n)) {
      if (r==c) next
      s <- clean_text(as.character(block[r,score_cols[c]]))
      if(is.na(s)) next
      m <- capture("^([0-9]+)\\s*[-\u2013\u2014:]\\s*([0-9]+)$", s)
      if (length(m)) {
        k <- k+1L
        out[[length(out)+1L]] <- data.table(Home=teams[r], Away=teams[c],
          Score=paste(m[2:3],collapse="-"), Stage=heading, WikiTable=i)
      }
    }
    audit[[length(audit)+1L]] <- data.table(Table=i,Stage=heading,Teams=n,
      PossibleCells=n*(n-1L),ParsedScores=k)
  }
  # Individual match reports are used for playoff fixtures outside matrices.
  for(tab in xml_find_all(doc,"//table[contains(concat(' ',normalize-space(@class),' '),' vevent ')]")) {
    row <- xml_find_first(tab,".//tr[1]"); cells <- xml_find_all(row,"./td")
    if(length(cells)<4L) next
    teams <- clean_text(xml_text(xml_find_all(row,".//*[contains(concat(' ',normalize-space(@class),' '),' org ')]")))
    if(length(teams)!=2L) next
    s <- clean_text(xml_text(cells[[3]])); m <- capture("^([0-9]+)\\s*[-\u2013:]\\s*([0-9]+)$",s)
    if(!length(m)) next
    score <- paste(m[2:3],collapse="-")
    existing <- rbindlist(out,fill=TRUE)
    if(nrow(existing) && any(existing$Home==teams[1] & existing$Away==teams[2] & existing$Score==score)) next
    heading <- clean_text(xml_text(xml_find_first(tab,"preceding::*[self::h2 or self::h3 or self::h4][1]")))
    out[[length(out)+1L]] <- data.table(Home=teams[1],Away=teams[2],Score=score,Stage=heading,WikiTable=NA_integer_)
  }
  games <- if(length(out)) rbindlist(out,fill=TRUE) else empty_games()
  games <- wiki_link_identities(games,doc)
  list(games=games,
       tables=rbindlist(audit,fill=TRUE))
}

# Wikipedia can label the same linked club differently in its regular-season
# and playoff tables. An identical article target supplies explicit identity
# evidence; ambiguous labels and clubs shown playing each other are excluded.
wiki_link_identities <- function(games,doc) {
  if(!nrow(games)) return(games)
  games <- copy(games)
  games[, `:=`(WikiHomeLabel=Home,WikiAwayLabel=Away,
    Home=clean_team_name(Home),Away=clean_team_name(Away))]
  clubs <- unique(c(games$Home,games$Away))
  links <- xml_find_all(doc,"//table//a[@href]")
  labels <- clean_team_name(xml_text(links))
  href <- sub("#.*$","",xml_attr(links,"href"))
  href <- sub("^https?://en.wikipedia.org/wiki/|^/wiki/|^[.]/","",href)
  evidence <- unique(data.table(Label=labels,Page=href)[Label %in% clubs &
    !is.na(Page) & nzchar(Page) & !grepl("[:?]",Page)])
  evidence <- evidence[!Label %in% evidence[,.N,by=Label][N>1L,Label]]
  for(page in unique(evidence$Page)) {
    group <- evidence[Page==page,Label]
    if(length(group)<2L || any(games$Home %in% group & games$Away %in% group)) next
    canonical <- group[order(-nchar(group),group)][1L]
    games[Home %in% group,Home:=canonical]
    games[Away %in% group,Away:=canonical]
  }
  games
}

# An explicit year always wins. A season without an explicit year uses its
# calendar format, not the order of rounds (postponements break chronological order).
rsssf_date <- function(x, start, end, year_hint=NA_integer_) {
  # The leading date is the actual playing date when the note explicitly says
  # postponed FROM. Never strip postponed TO, which names a different date.
  x <- sub("(?i);\\s*postponed from\\b.*$","",x,perl=TRUE)
  # Two-digit years are accepted only when they identify one season year.
  short_year <- capture("^\\s*([0-9]{1,2})[./-]([0-9]{1,2})[./-]([0-9]{2})[.]?\\s*$",x)
  if(length(short_year)) {
    years <- unique(c(start,end)); years <- years[years%%100L==as.integer(short_year[4])]
    if(length(years)!=1L) return(as.Date(NA))
    x <- paste(short_year[2],short_year[3],years,sep=".")
  }
  year_first <- capture("(?i)^\\s*([12][0-9]{3})[-/.]([A-Za-z]{3,9}|[0-9]{1,2})[-/.]([0-9]{1,2})\\s*$",x)
  if(length(year_first)) x <- paste(year_first[4],year_first[3],year_first[2])
  roman <- capture("^\\s*([0-9]{1,2})[.]?\\s+([IVX]+)[.]?\\s+([12][0-9]{3})[.]?\\s*$", x)
  if(length(roman)) {
    month <- match(roman[3],c("I","II","III","IV","V","VI","VII","VIII","IX","X","XI","XII"))
    if(!is.na(month)) x <- sprintf("%s.%d.%s",roman[2],month,roman[4])
  }
  # Some round dates append attendance metadata: [May 24. Total Att: ...].
  # Strip only this explicit metadata, not arbitrary text or date ranges.
  x <- sub("(?i)[.;] +(?:total +)?att(?:endance)?\\b.*$","",x,perl=TRUE)
  # Several RSSSF country files append a weekday to every date heading, for
  # example "[Jan 25, Thu]".  The weekday is useful to a reader but is not
  # part of the date and previously caused every such heading to resolve NA.
  x <- sub("(?i),?\\s+(?:Mon|Tue|Wed|Thu|Fri|Sat|Sun)(?:day)?[.]?\\s*$","",x,perl=TRUE)
  x <- trimws(gsub("[,\\[\\]]", " ", x))
  m <- capture("(?i)^([A-Za-z]{3,9})\\s+([0-9]{1,2})(?:\\s+([0-9]{4}))?$", x)
  if (length(m)) { mon <- match(tolower(substr(m[2],1,3)),tolower(month.abb)); day <- as.integer(m[3]); year <- m[4] }
  else {
    m <- capture("(?i)^([0-9]{1,2})[ ./-]+([A-Za-z]{3,9}|[0-9]{1,2})(?:[ ./-]+([0-9]{4}))?[.]?$", x)
    if (!length(m)) return(as.Date(NA))
    day <- as.integer(m[2]); mon <- suppressWarnings(as.integer(m[3]))
    if (is.na(mon)) mon <- match(tolower(substr(m[3],1,3)),tolower(month.abb))
    year <- m[4]
  }
  if (is.na(mon) || mon<1 || mon>12) return(as.Date(NA))
  # COVID extensions crossed the normal season boundary. Without an explicit
  # year, July/August/September can belong to either year; leave for review.
  if (!nzchar(year) && start==2019L && end==2020L && mon %in% 7:9) {
    if(is.na(year_hint)) return(as.Date(NA))
    year <- as.character(year_hint)
  }
  if (!nzchar(year)) year <- if(start==end || mon>=7L) start else end
  ans <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d",as.integer(year),mon,day),format="%Y-%m-%d"))
  if (!is.na(ans) && (as.integer(format(ans,"%Y"))<start || as.integer(format(ans,"%Y"))>end)) return(as.Date(NA))
  ans
}

# Recognise calendar vocabulary, not any word followed by a number. A scorer
# such as [Ivanovski 65] is a note. A real date range remains a heading, with
# an unresolved exact date, so it cannot inherit the preceding day's date.
rsssf_date_heading <- function(x) {
  months <- "(?:Jan(?:uary)?|Feb(?:ruary)?|Mar(?:ch)?|Apr(?:il)?|May|Jun(?:e)?|Jul(?:y)?|Aug(?:ust)?|Sep(?:t(?:ember)?)?|Oct(?:ober)?|Nov(?:ember)?|Dec(?:ember)?)"
  x <- trimws(x)
  grepl(paste0("(?i)^(?:",months," +[0-9]{1,2}(?:$|[ ,./-])|[0-9]{1,2}[ ./-]+",months,"(?:$|[ ,./-])|[0-9]{1,2}[./-][0-9]{1,2}(?:$|[ ,./-])|[12][0-9]{3}[-/.](?:",months,"|[0-9]{1,2})[-/.][0-9]{1,2}\\s*$)"),x,perl=TRUE)
}

rsssf_benign_note <- function(x) {
  grepl("(?i)^(?:in |at |played (?:in|at) |behind closed doors$|closed doors$|neutral (?:ground|venue)$|att(?:endance)?[.: ])",trimws(x),perl=TRUE) &
    !grepl("(?i)awarded|abandon|replay|void|annul|postpon|resched|original|instead|aet|penalt",x,perl=TRUE)
}

# Recover ambiguous summer years from the nearest dated matches on each side
# within the SAME phase. Do not infer a rollover just because a month decreases:
# RSSSF lists postponed games under their original round. Conflicting anchors
# remain unresolved. Use only original dates as anchors, never inferred dates.
resolve_summer_years <- function(games, start, end) {
  games <- copy(games)
  games[, DateBasis:=ifelse(is.na(Date),"unresolved","date_heading_and_season")]
  if(start!=2019L || end!=2020L || !nrow(games)) return(games)
  original <- games$Date
  for(i in which(is.na(original) & !games$Annotated & nzchar(games$DateHeading))) {
    candidates <- which(!is.na(original) & games$RSSSFPhase==games$RSSSFPhase[i])
    before <- candidates[candidates<i]; after <- candidates[candidates>i]
    neighbors <- c(if(length(before)) tail(before,1L),if(length(after)) head(after,1L))
    years <- unique(as.integer(format(original[neighbors],"%Y")))
    if(length(years)!=1L) next
    recovered <- rsssf_date(games$DateHeading[i],start,end,year_hint=years[1])
    if(!is.na(recovered)) games[i,`:=`(Date=recovered,
      DateBasis=paste0("same_phase_neighbor_year:",years[1],"; evidence_rows:",paste(neighbors,collapse=",")))]
  }
  games
}

# RSSSF sometimes continues a bracketed explanation in the same fixed-width
# block as the next fixtures. Once a club has been seen on the page, use the
# longest known club name at the start of the away field to trim that prose.
trim_rsssf_away <- function(away, known) {
  away <- trimws(away)
  if(!nzchar(away)) return(away)
  # Some detailed match lists append the stadium in parentheses with only one
  # space. Drop it when the club before that suffix has already appeared.
  parenthetical_club <- trimws(sub("\\s+\\([^()]+\\)$", "", away, perl=TRUE))
  if(parenthetical_club != away && parenthetical_club %in% known) return(parenthetical_club)
  # Side notes occupy a separate fixed-width column. Never shorten an ordinary
  # single-spaced club name just because another club shares its first word.
  boundary_match <- regexpr("[[:blank:]]{2,}",away,perl=TRUE)
  boundary <- boundary_match[1L]
  if(boundary<1L) return(away)
  candidate <- trimws(substr(away,1L,boundary-1L))
  remainder <- trimws(substr(away,boundary+attr(boundary_match,"match.length"),nchar(away)))
  if(candidate %in% known) return(candidate)
  # Modern RSSSF files often add a fixed-width venue column immediately after
  # the away club.  Argentine pages use place strings such as
  # "Alfredo Terrera, Santiago del Estero, G".  Strip that column even on the
  # first fixture, before a list of known clubs has been accumulated.
  if(rsssf_fixture_team(candidate) &&
     (grepl(",",remainder,fixed=TRUE) || startsWith(remainder,"("))) return(candidate)
  away
}

# Apply the same competition boundary rules to DOM and plain-text headings.
# Retain promotion/relegation playoffs, which Wikipedia may include.
rsssf_other_competition <- function(x) {
  x <- tolower(stringi::stri_trans_general(clean_text(x),"Latin-ASCII"))
  if(grepl("\\b(pohar|kauss|superkauss|karikas|superkarikas|kubok|kuboku|esiliiga|trofeo)\\b",x,perl=TRUE)) return(TRUE)
  x <- sub("^[12][0-9]{3}(?:/[0-9]{2,4})? +","",x,perl=TRUE)
  grepl("\\b(cup|puchar|cupen|cupa|kup|kupa|kupi|pokal|trophy|coppa|supercoppa|taure|women|superettan|ettan|obos|ykkonen)\\b|\\b(second|third|fourth|fifth|[2-9](?:nd|rd|th)) (level|division|league)\\b|\\bdivision [2-9]\\b|^(?:[2-9][.]? +hnl|[12][.] +divisjon|(?:lff +)?(?:i|ii|iii) +lyga|(?:i|ii|iii) +liga|liga (?:ii|iii))\\b",x,perl=TRUE)
}

# Some older RSSSF pages keep a whole country's season in one PRE block.  The
# domestic cup heading can then follow the top-flight fixture list without an
# H3/H4 boundary.  These are heading-only forms (never fixture lines), so they
# are safe hard stops for the league parser across the local spellings used by
# RSSSF: Svenska Cupen, Puchar Polski, Copa ..., Magyar Kupa, etc.
rsssf_cup_heading <- function(x) {
  x <- tolower(stringi::stri_trans_general(clean_text(x), "Latin-ASCII"))
  x <- sub("^[12][0-9]{3}(?:/[0-9]{2,4})? +", "", x, perl=TRUE)
  # Parenthetical sponsors/local translations are common, e.g. "Romania Cup
  # (Cupa României-Tuborg) 1999/2000". They are a heading annotation, not a
  # reason to carry the subsequent cup rounds into the preceding league.
  x <- trimws(sub("\\s*\\([^)]*\\)", "", x, perl=TRUE))
  grepl("^(?:[a-z]+ +){0,5}(?:cup|cupen|puchar|pohar|kupa|kup|pokal|cupa|coppa|taure|trophy|kauss|karikas|kubok|kuboku|superkauss|superkarikas)(?: +[a-z]+){0,3}(?: +(?:[12][0-9]{3}|[0-9]{2})(?:/[0-9]{2,4})?)? *$", x, perl=TRUE)
}

# A cross-table can contain only one score among unplayed '-' cells. Checking
# for multiple scores alone misses these rows and invents false team aliases.
# Attached hyphens in real names (Pen-y-Bont) remain valid.
rsssf_fixture_team <- function(x) {
  x <- trimws(x)
  grepl("[[:alpha:]]",x) &
    !grepl("\\b[0-9]+\\s+[0-9]+\\s+[0-9]+\\s+[0-9]+\\b",x,perl=TRUE) &
    !grepl("[0-9]+\\s*[-:]\\s*[0-9]+|(?:^|\\s)[-\u2013\u2014]+(?:\\s|$)|[,\\t]",x,perl=TRUE)
}

rsssf_games <- function(doc, start, end) {
  out <- list(); rejected <- list(); known_teams <- character()
  scanned <- character()
  # Navigation links are not competition boundaries. Work on a copy so callers
  # retain their original evidence document.
  doc <- read_html(as.character(doc))
  xml_remove(xml_find_all(doc,"//pre//a[starts-with(@href, '#')]"))
  # Named section anchors inside PRE retain explicit phase information even
  # when the visible heading uses another language.
  for(anchor in xml_find_all(doc,"//pre//a[@name='champ' or @name='releg']")) {
    xml_text(anchor) <- if(xml_attr(anchor,"name")=="champ") "\nChampionship Group\n" else "\nRelegation Group\n"
  }
  # DOM boundaries prevent dates leaking from one competition into another.
  nodes <- xml_find_all(doc,"//pre | //h2 | //h3 | //h4 | //b[a[@name] and not(ancestor::pre)]")
  section <- ""; stage <- ""; active <- TRUE; phase <- "regular"
  for (node in nodes) {
    if (xml_name(node)!="pre") {
      section <- clean_text(xml_text(node)); stage <- section
      if(phase_key(section)!="regular") phase <- phase_key(section)
      if(grepl("(?i)^regular",section)) phase <- "regular"
      if(rsssf_other_competition(section)) active <- FALSE
      next
    }
    if (!active) next
    date <- as.Date(NA); date_heading <- ""; in_note <- FALSE
    after_scorers <- FALSE
    lines <- strsplit(xml_text(node),"\n",fixed=TRUE)[[1]]
    for (j in seq_along(lines)) {
      if(j %% 500L==0L) message("    RSSSF lines: ",j,"/",length(lines),
        "; extracted fixtures: ",length(out))
      line <- trimws(lines[j]); if (!nzchar(line)) next
      line <- gsub("\u00a0"," ",line,fixed=TRUE)
      # In a handful of legacy country pages, the cup has no heading at all.
      # It begins only after the completed league's scorer list. A new knockout
      # "First Round" after that list therefore cannot belong to the league.
      if(grepl("(?i)^(?:top\\s*)?goalscorers?\\s*:?$|^top\\s+scorers?\\s*:?$",line,perl=TRUE)) {
        after_scorers <- TRUE
        next
      }
      if(after_scorers && grepl("(?i)^(?:first|1st|[1-9][./])\\s+round\\b",line,perl=TRUE)) {
        message("    End of top-flight block: post-scorers knockout round")
        active <- FALSE
        break
      }
      # A missing opening bracket is harmless only for a complete single date.
      if(grepl("^[A-Za-z]+ +[0-9]{1,2}(?: +[0-9]{4})?\\]$",line,perl=TRUE) &&
         !is.na(rsssf_date(sub("\\]$","",line),start,end))) line <- paste0("[",line)
      # Typographical score separators carry exactly the same score semantics.
      line <- gsub("[\u2013\u2014\u2212]","-",line,perl=TRUE)
      # Administrative fixture outcomes are not date/section boundaries. Keep
      # the heading for subsequent played matches, without dating the award.
      if(grepl("^.+?\\s+(?:awd|ppd|pst|postp|abd|canc|ann)[.]?\\s+.+$",line,perl=TRUE,ignore.case=TRUE)) next
      # Excluded-team standings and prose can contain a score-shaped token.
      if(grepl("^(?:-[.]|NB:|all remaining matches\\b)",line,perl=TRUE,ignore.case=TRUE)) {
        date <- as.Date(NA); date_heading <- ""
        next
      }
      # Competition headings can be plain text inside one enormous PRE block.
      # Stop at explicit cup/lower-level sections rather than treating their
      # fixtures and result matrices as more top-flight teams.
      boundary_line <- sub("^[12][0-9]{3}(?:/[0-9]{2,4})? +","",line,perl=TRUE)
      cup_heading <- tolower(stringi::stri_trans_general(boundary_line,"Latin-ASCII"))
      if(rsssf_cup_heading(cup_heading)) {
        active <- FALSE
        break
      }
      if(nchar(boundary_line)<100L && !grepl("[0-9]+\\s*[-:]\\s*[0-9]+|[.;:]|\\[|\\]",boundary_line,perl=TRUE) &&
         grepl("(?i)^(second|third|fourth|fifth|[2-9](?:nd|rd|th)) |^(?:[A-Za-z]+ +){0,4}(?:cup|kup|pokal|taure)(?: +[12][0-9]{3}(?:/[0-9]{2,4})?)? *$",boundary_line,perl=TRUE) &&
         rsssf_other_competition(boundary_line) &&
         !grepl("(?i)\\b(qualified|relegated|promoted|winner|teams|played|licen[cs]e)\\b",boundary_line,perl=TRUE)) {
        message("    End of top-flight block: ",line)
        active <- FALSE
        break
      }
      scanned <- c(scanned,line)
      # Scorers, attendance and weather are bracketed notes, sometimes spanning
      # several lines. They neither become fixtures nor erase the current date.
      if(in_note) {
        looks_fixture <- grepl("^.+?\\s+[0-9]{1,2}\\s*[-:]\\s*[0-9]{1,2}\\s+.+$",line,perl=TRUE)
        leading_date <- capture("^\\[([^]]+)\\]",line)
        is_date <- length(leading_date)>0L && rsssf_date_heading(leading_date[2])
        if(!looks_fixture && !is_date) {
          in_note <- !grepl("]",line,fixed=TRUE)
          next
        }
        in_note <- FALSE
      }
      line_had_date <- FALSE
      # Some scorer notes have an opening brace and closing square bracket.
      # Preserve the date while skipping the entire note, including continuations.
      if(startsWith(line,"{")) {
        in_note <- !grepl("[}\\]]",line,perl=TRUE)
        next
      }
      # E.g. '1 . round (22. VII. 2011.)'. Read before standings-row rejection.
      roman_round <- capture("(?i)^([0-9]+)\\s*[.]?\\s*round\\s*\\(([^)]+)\\)\\s*$",line)
      if(length(roman_round)) {
        stage <- paste("Round",roman_round[2]); date_heading <- roman_round[3]
        date <- rsssf_date(date_heading,start,end)
        next
      }
      # Alternative fixture layout: Home - Away 2:1 (optional actual date).
      tail_score <- capture("^(.+?)\\s+[-\u2013]\\s+(.+?)\\s+([0-9]{1,2})\\s*[-:]\\s*([0-9]{1,2})(?:\\s+\\(([^)]+)\\))?\\s*$",line)
      if(length(tail_score)) {
        # Matrix rows and two-leg aggregate summaries are not single fixtures.
        clubs <- tail_score[2:3]
        if(!all(rsssf_fixture_team(clubs))) next
        match_date <- date
        match_heading <- date_heading
        if(nzchar(tail_score[6])) {
          match_heading <- tail_score[6]
          match_date <- rsssf_date(match_heading,start,end)
        }
        out[[length(out)+1L]] <- data.table(RHome=trimws(tail_score[2]),RAway=trimws(tail_score[3]),
          Score=paste(tail_score[4:5],collapse="-"),Date=match_date,
          DateHeading=match_heading,Annotated=FALSE,MatchNote="",
          RSSSFSection=section,RSSSFStage=stage,RSSSFPhase=phase,SourceLine=j,RawLine=lines[j])
        known_teams <- unique(c(known_teams,trimws(tail_score[2]),trimws(tail_score[3])))
        next
      }
      if(startsWith(line,"[")) {
        bracket <- capture("^\\[([^]]+)\\]",line)
        looks_date <- length(bracket) && rsssf_date_heading(bracket[2])
        if(!looks_date) {in_note <- !grepl("]",line,fixed=TRUE); next}
      }
      if(grepl("^[0-9]+[.]\\s*[^ ]",line) && !rsssf_date_heading(line)) {date <- as.Date(NA); date_heading <- ""; next}
      if(grepl("(?i)^(championship|relegation|play.off|play.out)",line,perl=TRUE) && phase_key(line)!="regular") phase <- phase_key(line)
      if (grepl("(?i)^(round|runda|omgang|matchday|championship|relegation|play.?off|regular season|group)\\b",line,perl=TRUE)) {
        stage <- line; date <- as.Date(NA); date_heading <- ""
        # Some round headings carry their own bracketed date.
      }
      # A bracketed calendar date can also be embedded in a stage heading, for
      # example "Preliminary Round [Apr 30]" or "First Legs [May 10]".
      # Trailing brackets on fixture rows remain match notes, not dates.
      dm <- capture("^\\[([^]]+)\\]",line)
      if(!length(dm)) {
        embedded <- capture("\\[([^]]+)\\]",line)
        fixture_on_line <- grepl("^.+?\\s+[0-9]{1,2}\\s*[-:]\\s*[0-9]{1,2}\\s+.+$",line,perl=TRUE)
        if(length(embedded) && rsssf_date_heading(embedded[2]) && !fixture_on_line) dm <- embedded
      }
      if (length(dm)) {
        line_had_date <- TRUE
        date_heading <- dm[2]
        date <- rsssf_date(dm[2],start,end)
        line <- trimws(sub("\\[[^]]+\\]", "",line))
        if(nzchar(line) && !grepl("[0-9]{1,2}\\s*[-:]\\s*[0-9]{1,2}",line,perl=TRUE)) stage <- line
      } else if (grepl("(?i)^(Jan(?:uary)?|Feb(?:ruary)?|Mar(?:ch)?|Apr(?:il)?|May|Jun(?:e)?|Jul(?:y)?|Aug(?:ust)?|Sep(?:tember)?|Oct(?:ober)?|Nov(?:ember)?|Dec(?:ember)?) +[0-9]{1,2}(?:$|[, ]+[0-9]{4}|\\s+[A-Za-z])",line,perl=TRUE) ||
                 grepl("^[0-9]{1,2}[./][0-9]{1,2}",line)) {
        prefix <- capture("^([A-Za-z]+ +[0-9]{1,2}(?:,? +[0-9]{4})?|[0-9]{1,2}[./][0-9]{1,2}(?:[./](?:[0-9]{4}|[0-9]{2}))?)[.:]?\\s*(.*)$",line)
        if(length(prefix)) {
          # A full date followed by venue/attendance prose describes the
          # preceding detailed match. It must never date the NEXT fixture.
          if(nzchar(trimws(prefix[3])) && !grepl("^.+?\\s+[0-9]{1,2}\\s*[-:]\\s*[0-9]{1,2}\\s+.+$",prefix[3],perl=TRUE)) {
            date <- as.Date(NA); date_heading <- ""
            next
          }
          line_had_date <- TRUE; date_heading <- prefix[2]; date <- rsssf_date(prefix[2],start,end); line <- prefix[3]
        }
      }
      m <- capture("^(.+?)\\s+([0-9]{1,2})\\s*[-:]\\s*([0-9]{1,2})\\s+(.+?)\\s*$",line)
      if (!length(m)) {
        if(grepl("[",line,fixed=TRUE) && !grepl("]",line,fixed=TRUE)) in_note <- TRUE
        looks_resultish <- grepl("^.+?\\s+(?:[0-9]{1,2}|awd)\\s*[-:]\\s*(?:[0-9]{1,2}|[A-Za-z]+)\\s+.+$",line,perl=TRUE)
        if(nzchar(line) && !line_had_date && !looks_resultish && !grepl("(?i)^round|^runda|^omgang|^matchday",line,perl=TRUE)) {date <- as.Date(NA); date_heading <- ""}
        next
      }
      notes <- regmatches(m[5],gregexpr("\\[[^]]*\\]",m[5],perl=TRUE))[[1L]]
      # A results matrix row has several score cells, not a single away club.
      if(grepl("[0-9]+\\s*[-:]\\s*[0-9]+",sub("\\[.*$","",m[5]),perl=TRUE)) next
      note_text <- gsub("^\\[|\\]$","",notes)
      annotation <- paste(note_text,collapse="; ")
      away <- trimws(sub("\\s*\\[.*$","",m[5]))
      away <- trim_rsssf_away(away,known_teams)
      if(!all(rsssf_fixture_team(c(m[2],away)))) next
      # Extra time and penalty-shootout notes describe completed matches and
      # retain their dates. Only administrative outcomes make a played result
      # unsafe for the historical candidate.
      administrative <- grepl("(?i)awarded|abandon|void|annul|postpon|resched|cancel|not played|originally",annotation,perl=TRUE) ||
        grepl("(?i)(?:^|\\s)(awarded|abandoned|void|annulled|postponed|cancelled)(?:\\s|$)",away,perl=TRUE)
      annotated <- administrative
      out[[length(out)+1L]] <- data.table(RHome=trimws(m[2]),RAway=away,
        Score=paste(m[3:4],collapse="-"),Date=if(annotated) as.Date(NA) else date,
        DateHeading=date_heading,Annotated=annotated,MatchNote=annotation,
        RSSSFSection=section,RSSSFStage=stage,RSSSFPhase=phase,SourceLine=j,RawLine=lines[j])
      known_teams <- unique(c(known_teams,trimws(m[2]),away))
      if(grepl("[",lines[j],fixed=TRUE) && !grepl("]",lines[j],fixed=TRUE)) in_note <- TRUE
    }
  }
  result <- if(length(out)) resolve_summer_years(unique(rbindlist(out)),start,end) else data.table(RHome=character(),RAway=character(),Score=character(),Date=as.Date(character()))
  date_lines <- sum(vapply(scanned,function(x) {
    if(grepl("^[0-9]+[.]\\s*.+?\\s+[0-9]+\\s+[0-9]+\\s+[0-9]+\\s+[0-9]+\\s+[0-9]+[-:][0-9]+",x,perl=TRUE)) return(FALSE)
    b <- capture("\\[([^]]+)\\]",x)
    rsssf_date_heading(x) || (length(b)>0L && rsssf_date_heading(b[2]))
  },logical(1)))
  matrix_lines <- sum(lengths(regmatches(scanned,gregexpr("[0-9]+[-:][0-9]+",scanned,perl=TRUE)))>=3L)
  attr(result,"source_assessment") <- if(nrow(result)) "fixture_rows_extracted" else
    if(date_lines>0L) "date_headings_present_but_fixture_layout_unresolved" else
    if(matrix_lines>0L) "undated_result_grid_detected_no_league_date_headings" else
    "no_fixture_rows_or_date_headings_detected_in_league_section"
  result
}

# Name similarity proposes identities; independent season results confirm them.
# No substring matching: "Arka" cannot match the token "Siarka".
name_tokens <- function(x) {
  z <- strsplit(team_key(x)," ",fixed=TRUE)[[1]]
  z <- z[!z %in% c("fc","fk","if","ifk","bk","sk","ks","afc","ac","cf","ff","aif","club")]
  z[nzchar(z)]
}
name_related <- function(a,b, x=name_tokens(a), y=name_tokens(b)) {
  if(!length(x) || !length(y)) return(FALSE)
  equivalent <- function(u,v) u==v ||
    (min(nchar(u),nchar(v))>=4L && substr(u,1,1)==substr(v,1,1) &&
      as.numeric(adist(u,v))<=if(min(nchar(u),nchar(v))>=6L) 2L else 1L) ||
    (nchar(u)>=5L && paste0(u,"s")==v) ||
    (nchar(v)>=5L && paste0(v,"s")==u) ||
    (nchar(u)==1L && startsWith(v,u)) || (nchar(v)==1L && startsWith(u,v))
  # All tokens in the shorter name must be explained; initials alone are weak.
  if(length(x)>length(y)) {tmp<-x;x<-y;y<-tmp}
  any(nchar(x)>=3L) && all(vapply(x,function(u) any(vapply(y,function(v) equivalent(u,v),logical(1))),logical(1)))
}

map_teams <- function(raw, wiki, aliases=data.table(), r=NULL, w=NULL,
                      season_start=NA_integer_) {
  raw <- unique(raw); wiki <- unique(wiki)
  mapping <- data.table(RSSSF=raw,Wikipedia=rep(NA_character_,length(raw)),
    Method=rep("review",length(raw)),Suggested=rep(NA_character_,length(raw)),
    Support=integer(length(raw)),Compared=integer(length(raw)),Opponents=integer(length(raw)),
    RunnerUp=integer(length(raw)))
  if(!length(raw)) return(mapping)
  for(i in seq_along(raw)) {
    hit <- which(team_key(wiki)==team_key(raw[i])); method <- "exact"
    if(nrow(aliases)) {
      manual <- aliases[team_key(RSSSF)==team_key(raw[i]) & Wikipedia %in% wiki &
        (is.na(StartYear) | is.na(season_start) | StartYear<=season_start) &
        (is.na(EndYear) | is.na(season_start) | EndYear>=season_start),unique(Wikipedia)]
      if(length(manual)) {hit <- match(manual,wiki);method <- "manual"}
    }
    if(length(hit)==1L) mapping[i,`:=`(Wikipedia=wiki[hit],Method=method)]
  }
  # Reject collisions even among seed names; a human can inspect team_map.csv.
  collisions <- mapping[!is.na(Wikipedia),.N,by=Wikipedia][N>1L, Wikipedia]
  mapping[Wikipedia %in% collisions,`:=`(Wikipedia=NA_character_,Method="identity_collision")]
  if(is.null(r) || is.null(w) || !nrow(r) || !nrow(w)) return(mapping)
  evidence <- if("Annotated" %in% names(r)) unique(r[Annotated == FALSE,.(RHome,RAway,Score)]) else unique(r[,.(RHome,RAway,Score)])
  # Transliteration and tokenization are invariant across candidate pairs.
  raw_tokens <- lapply(raw,name_tokens)
  wiki_tokens <- lapply(wiki,name_tokens)
  related <- vapply(seq_along(wiki),function(wi) vapply(seq_along(raw),function(ri)
    name_related(raw[ri],wiki[wi],raw_tokens[[ri]],wiki_tokens[[wi]]),logical(1)),logical(length(raw)))
  related <- matrix(related,nrow=length(raw),ncol=length(wiki))
  result_keys <- lapply(wiki,function(candidate) unique(c(
    paste(TRUE,w$Away[w$Home==candidate],w$Score[w$Home==candidate],sep="\t"),
    paste(FALSE,w$Home[w$Away==candidate],w$Score[w$Away==candidate],sep="\t"))))
  log <- list()
  for(pass in seq_len(length(raw))) {
    unresolved <- which(is.na(mapping$Wikipedia))
    if(!length(unresolved)) break
    proposals <- list()
    # Evaluate a whole pass against a fixed set of already accepted identities.
    # Unique whole-token name candidates can provide provisional opponent
    # evidence even when a page abbreviates EVERY club. These do not themselves
    # become accepted identities; each still needs corroborating results.
    anchors <- mapping$Wikipedia
    for(ai in which(is.na(anchors))) {
      possible <- which(related[ai,])
      if(length(possible)==1L) anchors[ai] <- wiki[possible]
    }
    collided <- !is.na(anchors) & (duplicated(anchors) | duplicated(anchors,fromLast=TRUE))
    anchors[collided & is.na(mapping$Wikipedia)] <- NA_character_
    home_map <- anchors[match(evidence$RHome,mapping$RSSSF)]
    away_map <- anchors[match(evidence$RAway,mapping$RSSSF)]
    for(i in unresolved) {
      name <- raw[i]
      games <- rbind(
        data.table(Opponent=away_map[evidence$RHome==name],Score=evidence$Score[evidence$RHome==name],AtHome=TRUE),
        data.table(Opponent=home_map[evidence$RAway==name],Score=evidence$Score[evidence$RAway==name],AtHome=FALSE))
      games <- unique(games[!is.na(Opponent)])
      if(!nrow(games)) next
      game_keys <- paste(games$AtHome,games$Opponent,games$Score,sep="\t")
      candidates <- rbindlist(lapply(seq_along(wiki),function(wi) {
        candidate <- wiki[wi]
        ok <- game_keys %in% result_keys[[wi]]
        data.table(RSSSF=name,Wikipedia=candidate,Support=sum(ok),Compared=nrow(games),
          Opponents=length(unique(games$Opponent[ok])),NameRelated=related[i,wi],Pass=pass)
      }))
      setorder(candidates,-Support,-Opponents)
      log[[length(log)+1L]] <- candidates
      best <- candidates[1]; runner <- if(nrow(candidates)>1L) candidates$Support[2] else 0L
      mapping[i,`:=`(Suggested=best$Wikipedia,Support=best$Support,Compared=best$Compared,
        Opponents=best$Opponents,RunnerUp=runner)]
      # Name-related candidates need a modest result fingerprint. A club can
      # be known in RSSSF by a historic sponsor/short name with no shared token
      # (e.g. RSSSF "Termalica" versus Wikipedia "Nieciecza"). In that case,
      # accept only a strong season-wide result fingerprint: at least ten
      # results against five opponents, 80% agreement, and a clear lead over
      # the runner-up. This remains whole-season evidence, never substring
      # matching or a one-game exception.
      accepted <- if(best$NameRelated) best$Support>=3L && best$Opponents>=2L &&
        best$Support/best$Compared>=0.8 && best$Support-runner>=2L else
        best$Support>=10L && best$Opponents>=5L && best$Support/best$Compared>=0.8 && best$Support-runner>=5L
      if(accepted && !best$Wikipedia %in% mapping$Wikipedia) proposals[[length(proposals)+1L]] <-
        data.table(Index=i,Wikipedia=best$Wikipedia,Method=if(best$NameRelated) "name_and_results" else "season_results")
    }
    if(!length(proposals)) break
    proposals <- rbindlist(proposals)
    # Two RSSSF identities competing for one Wikipedia club remain unresolved.
    proposals <- proposals[!duplicated(Wikipedia) & !duplicated(Wikipedia,fromLast=TRUE)]
    if(!nrow(proposals)) break
    for(j in seq_len(nrow(proposals))) mapping[proposals$Index[j],
      `:=`(Wikipedia=proposals$Wikipedia[j],Method=proposals$Method[j])]
  }
  attr(mapping,"candidate_audit") <- rbindlist(log,fill=TRUE)
  mapping
}

attach_dates <- function(w, r, aliases=data.table(), season_start=NA_integer_) {
  w <- copy(w)
  w[, `:=`(Home=clean_team_name(Home),Away=clean_team_name(Away))]
  # fread infers logical columns when every saved value is blank. Remove the
  # old output columns before assigning, so scalar recycling cannot coerce
  # dates or evidence strings into the previously inferred logical type.
  output_columns <- intersect(c("Date","DateStatus","RSSSFLine","MatchMethod"),names(w))
  if(length(output_columns)) w[, (output_columns):=NULL]
  w[, `:=`(Date=rep(as.Date(NA),.N),DateStatus=rep("no_matching_rsssf_result",.N),
    RSSSFLine=rep(NA_character_,.N),MatchMethod=rep(NA_character_,.N))]
  mapping <- map_teams(c(r$RHome,r$RAway),unique(c(w$Home,w$Away)),aliases,r,w,season_start)
  if(!nrow(r)) {
    w[,DateStatus:="rsssf_fixtures_not_extracted"]
    return(list(games=w,mapping=mapping,candidates=data.table()))
  }
  # A page may repeat the league in a summary and a detailed section, using
  # different spellings. Resolve within each section using the SAME evidence
  # thresholds, then accept only identities consistent across those sections.
  if("RSSSFSection" %in% names(r) && uniqueN(r$RSSSFSection)>1L) {
    section_maps <- rbindlist(lapply(unique(r$RSSSFSection),function(section_name) {
      block <- r[RSSSFSection==section_name]
      map_teams(c(block$RHome,block$RAway),unique(c(w$Home,w$Away)),aliases,block,w,season_start)
    }),fill=TRUE)
    for(nm in mapping[is.na(Wikipedia),RSSSF]) {
      proposals <- section_maps[RSSSF==nm & !is.na(Wikipedia)]
      if(nrow(proposals) && uniqueN(proposals$Wikipedia)==1L) {
        mapping[RSSSF==nm,`:=`(Wikipedia=proposals$Wikipedia[1L],Method="section_name_and_results")]
      }
    }
  }
  r <- copy(r)
  r[, Home:=mapping$Wikipedia[match(RHome,mapping$RSSSF)]]
  r[, Away:=mapping$Wikipedia[match(RAway,mapping$RSSSF)]]
  round_bounds <- function(s) capture("(?i)(?:matches|rounds) +([0-9]+)\\s*[-\u2013\u2014]\\s*([0-9]+)",s)
  # Some 'Matches 1-22' tables describe cycles, whereas RSSSF numbers a
  # differently ordered schedule. Validate ranges against unique fixtures
  # throughout this season before using them to disambiguate any match.
  range_checks <- logical()
  if("RSSSFStage" %in% names(r)) for(j in seq_len(nrow(w))) {
    b <- round_bounds(w$Stage[j]); if(!length(b)) next
    one <- r[!is.na(Home) & !is.na(Away) & Home==w$Home[j] & Away==w$Away[j] & Score==w$Score[j]]
    if(nrow(one)!=1L || is.na(one$Date)) next
    rn <- suppressWarnings(as.integer(sub("(?i)^Round +([0-9]+).*$","\\1",one$RSSSFStage,perl=TRUE)))
    if(!is.na(rn)) range_checks <- c(range_checks,rn>=as.integer(b[2]) && rn<=as.integer(b[3]))
  }
  ranges_supported <- length(range_checks)>=5L && all(range_checks)
  for(i in seq_len(nrow(w))) {
    if(w$DateStatus[i]=="matched") next
    hit <- r[!is.na(Home) & !is.na(Away) & Home==w$Home[i] & Away==w$Away[i] & Score==w$Score[i]]
    peers <- which(w$Home==w$Home[i] & w$Away==w$Away[i] & w$Score==w$Score[i])
    repeated <- length(peers)>1L
    explicit_phase <- grepl("(?i)regular season|first round|championship|relegation|play.off|play.out",w$Stage[i],perl=TRUE)
    if((repeated || (explicit_phase && any(hit$RSSSFPhase==phase_key(w$Stage[i])))) && "RSSSFPhase" %in% names(hit)) {
      phase <- phase_key(w$Stage[i]); hit <- hit[RSSSFPhase==phase]
      peers <- peers[phase_key(w$Stage[peers])==phase]
      repeated <- length(peers)>1L
    }
    # Wikipedia explicitly labels round ranges in multi-cycle leagues. A
    # matching score in another range is another fixture, not a second date
    # for this one. Require round evidence on every candidate before narrowing.
    bounds <- round_bounds(w$Stage[i])
    if(ranges_supported && length(bounds) && nrow(hit) && "RSSSFStage" %in% names(hit)) {
      rounds <- suppressWarnings(as.integer(sub("(?i)^Round +([0-9]+).*$","\\1",hit$RSSSFStage,perl=TRUE)))
      if(all(!is.na(rounds))) {
        lo <- as.integer(bounds[2]); hi <- as.integer(bounds[3])
        hit <- hit[rounds>=lo & rounds<=hi]
        peers <- peers[vapply(w$Stage[peers],function(s) {
          b <- capture("(?i)(?:matches|rounds) +([0-9]+)\\s*[-\u2013\u2014]\\s*([0-9]+)",s)
          length(b)>0L && identical(b[2:3],bounds[2:3])
        },logical(1))]
        repeated <- length(peers)>1L
      }
    }
    dates <- sort(unique(hit$Date[!is.na(hit$Date)]))
    if(repeated && length(dates)==length(peers) && all(!is.na(hit$Date))) {
      # Recover the complete multiset of identical Home/Away/Score records.
      # This proves the exported date/result tuples without guessing which
      # otherwise indistinguishable Wikipedia cell belongs to which date.
      # Original Stage remains source context, not asserted date alignment.
      w[peers,`:=`(Date=dates,DateStatus="matched",MatchMethod="complete_identical_fixture_date_set",
        RSSSFLine=vapply(dates,function(d) paste(unique(hit[Date==d,RawLine]),collapse=" | "),character(1)))]
    } else if(length(dates) && (repeated || length(dates)>1L)) {
      w[i,`:=`(DateStatus="ambiguous_repeated_fixture",MatchMethod="requires_occurrence_evidence")]
    } else if(length(dates)==1L) w[i,`:=`(Date=dates[1],DateStatus="matched",MatchMethod="exact_fixture",
      RSSSFLine=paste(unique(hit[Date==dates[1],RawLine]),collapse=" | "))]
    else if(nrow(hit)) w[i,DateStatus:="rsssf_date_missing_or_unresolved"]
    else if(nrow(r[!is.na(Home) & !is.na(Away) & Home==w$Home[i] & Away==w$Away[i]])) w[i,DateStatus:="score_disagreement"]
    else if(!w$Home[i] %in% mapping$Wikipedia || !w$Away[i] %in% mapping$Wikipedia) w[i,DateStatus:="team_identity_unresolved"]
  }
  list(games=w,mapping=mapping,candidates=attr(mapping,"candidate_audit"))
}

# Produce a human-reviewable alias report. This intentionally suggests aliases
# but never approves them. A zero RSSSF appearance is more likely to be a name
# problem; a partial appearance usually points to dates, scores, duplicates,
# or a parser issue instead.
alias_review_rows <- function(country, season, start, w, r, mapping) {
  wiki_names <- sort(unique(c(w$Home,w$Away)))
  rsssf_names <- sort(unique(c(r$RHome,r$RAway)))
  wiki_count <- data.table(Name=c(w$Home,w$Away))[,.(WikiGames=.N),by=Name]
  rsssf_count <- data.table(Name=c(r$RHome,r$RAway))[,.(RSSSFGames=.N),by=Name]
  rows <- lapply(wiki_names,function(nm) {
    mapped <- mapping[Wikipedia==nm & !is.na(Wikipedia),RSSSF]
    suggested <- mapping[Suggested==nm & !is.na(Suggested),RSSSF]
    mapped_count <- sum(rsssf_count[Name %in% mapped,RSSSFGames],na.rm=TRUE)
    wiki_games <- wiki_count[Name==nm,WikiGames][1L]
    status <- if(!nrow(r)) "rsssf_fixtures_not_extracted" else if(!length(mapped)) "wiki_team_not_mapped" else
      if(mapped_count < wiki_games) "partial_team_match" else "mapped"
    suggestion <- unique(c(mapped,suggested))
    suggestion <- suggestion[nzchar(suggestion)]
    data.table(Country=country,Season=season,StartYear=start,
      WikiName=nm,WikiGames=wiki_games,RSSSFName=paste(mapped,collapse=" | "),
      UnmatchedWikiGames=sum((w$Home==nm | w$Away==nm) & w$DateStatus!="matched"),
      RSSSFGames=mapped_count,AliasStatus=status,
      SuggestedRSSSFNames=paste(setdiff(suggestion,mapped),collapse=" | "),
      SuggestedWikipediaName=NA_character_,Support=NA_integer_,Compared=NA_integer_,
      Opponents=NA_integer_,RunnerUp=NA_integer_,ReviewReason=if(status=="rsssf_fixtures_not_extracted")
        "No RSSSF fixtures extracted; inspect source layout/date availability before alias review" else if(status=="wiki_team_not_mapped")
        "No mapped RSSSF identity; inspect suggested names" else if(status=="partial_team_match")
        "Team appears, but some fixtures remain unmatched" else "")
  })
  rows <- c(rows,lapply(which(is.na(mapping$Wikipedia)),function(i) {
    data.table(Country=country,Season=season,StartYear=start,WikiName=NA_character_,
      WikiGames=NA_integer_,RSSSFName=mapping$RSSSF[i],RSSSFGames=rsssf_count[Name==mapping$RSSSF[i],RSSSFGames][1L],
      AliasStatus="rsssf_team_not_mapped",SuggestedRSSSFNames=NA_character_,
      SuggestedWikipediaName=ifelse(is.na(mapping$Suggested[i]),"",mapping$Suggested[i]),
      Support=mapping$Support[i],Compared=mapping$Compared[i],
      Opponents=mapping$Opponents[i],RunnerUp=mapping$RunnerUp[i],
      ReviewReason="RSSSF identity has no accepted Wikipedia mapping")
  }))
  rbindlist(rows,fill=TRUE)
}

# Preserve high-coverage extracted results, not a claim of independently
# verified season completeness. Snapshots never overwrite the production CSV.
freeze_season <- function(out, row) {
  if(nrow(row)!=1L || is.na(row$WikipediaGames) || row$WikipediaGames<=0L ||
     is.na(row$DatedGames) || row$DatedGames/row$WikipediaGames<=0.95 ||
     is.na(row$Error) || nzchar(row$Error)) return(FALSE)
  if("ExtractionReview" %in% names(row) && !is.na(row$ExtractionReview) && nzchar(row$ExtractionReview)) return(FALSE)
  key <- paste(row$Country,gsub("/","-",row$Season),sep="_")
  target <- file.path(out,"locked_seasons",key)
  if(dir.exists(target)) return(TRUE)
  required <- c("all_wikipedia_games.csv","rsssf_evidence.csv","team_map.csv")
  paths <- file.path(out,key,required)
  if(!all(file.exists(paths))) return(FALSE)
  games <- fread(paths[1L])
  if(!all(c("Home","Away","Score","Date","DateStatus") %in% names(games))) return(FALSE)
  dated <- games[DateStatus=="matched"]
  years <- suppressWarnings(as.integer(substr(as.character(dated$Date),1L,4L)))
  end <- row$StartYear+as.integer(grepl("/",row$Season,fixed=TRUE))
  if(nrow(games)!=row$WikipediaGames || nrow(dated)!=row$DatedGames ||
     anyNA(dated$Date) || anyNA(years) || any(years<row$StartYear | years>end) ||
     anyDuplicated(dated[,.(Home,Away,Date)]) || any(dated$Home==dated$Away)) return(FALSE)
  dir.create(dirname(target),recursive=TRUE,showWarnings=FALSE)
  staging <- tempfile("pending_",tmpdir=dirname(target))
  dir.create(staging)
  on.exit(unlink(staging,recursive=TRUE),add=TRUE)
  stopifnot(all(file.copy(paths,file.path(staging,required))))
  snapshot <- copy(row)
  snapshot[, `:=`(LockedAt=format(Sys.time(),"%Y-%m-%d %H:%M:%S"),
    LockBasis="raw coverage >95%; structural checks; completeness not independently certified")]
  fwrite(snapshot,file.path(staging,"audit.csv"))
  names_to_hash <- c(required,"audit.csv")
  fwrite(data.table(File=names_to_hash,MD5=unname(tools::md5sum(file.path(staging,names_to_hash)))),
    file.path(staging,"checksums.csv"))
  if(!file.rename(staging,target)) stop("Could not finalize locked season: ",key)
  TRUE
}

read_season_locks <- function(out) {
  paths <- list.files(file.path(out,"locked_seasons"),pattern="^audit[.]csv$",recursive=TRUE,full.names=TRUE)
  rows <- lapply(paths,function(path) {
    folder <- dirname(path)
    hashes <- fread(file.path(folder,"checksums.csv"))
    actual <- unname(tools::md5sum(file.path(folder,hashes$File)))
    if(anyNA(actual) || !identical(actual,hashes$MD5)) stop("Locked season changed or incomplete: ",folder)
    row <- fread(path,colClasses=list(character="Error"))
    row[is.na(Error),Error:=""]
    row
  })
  rbindlist(rows,fill=TRUE)
}

run_four_leagues <- function(config_override=NULL,
    output_name="Wikipedia_RSSSF_Four_Leagues", env_prefix="FOUR_LEAGUES",
    run_label="Wikipedia + RSSSF four-league rebuild") {
  if(has_tictoc) {
    tictoc::tic(run_label)
    on.exit(tictoc::toc(),add=TRUE)
  }
  root <- normalizePath(Sys.getenv("J_RATINGS_REPO","C:/Users/stjuk/Documents/GitHub/J-Ratings"),winslash="/",mustWork=TRUE)
  out <- file.path(root,"EuropeanFootball/pipeline_data/Manual_Sources",output_name)
  cache <- file.path(out,"cache"); dir.create(cache,recursive=TRUE,showWarnings=FALSE)
  previous_audit <- file.path(out,"season_audit.csv")
  resume <- Sys.getenv(paste0(env_prefix,"_RESUME"),"1")=="1"
  saved_audit <- if(file.exists(previous_audit)) fread(previous_audit) else data.table()
  use_locks <- Sys.getenv(paste0(env_prefix,"_USE_LOCKS"),"1")=="1"
  locked_audit <- data.table()
  if(use_locks) {
    for(si in seq_len(nrow(saved_audit))) freeze_season(out,saved_audit[si])
    locked_audit <- read_season_locks(out)
    if(nrow(locked_audit)) {
      fwrite(locked_audit,file.path(out,"locked_seasons.csv"))
      saved_audit <- rbindlist(list(locked_audit,saved_audit[!RSSSF %in% locked_audit$RSSSF]),fill=TRUE)
    }
    message("Protected seasons above 95%: ",nrow(locked_audit)," (also skipped when RESUME=0).")
  }
  wiki_download_min <- as.integer(Sys.getenv(paste0(env_prefix,"_WIKI_DOWNLOAD_MIN_YEAR"),"2010"))
  if(file.exists(previous_audit)) file.copy(previous_audit,
    file.path(out,paste0("season_audit_before_",format(Sys.time(),"%Y%m%d_%H%M%S"),".csv")),overwrite=FALSE)
  cache_reads <- 0L; download_attempts <- 0L
  download_delay <- as.numeric(Sys.getenv(paste0(env_prefix,"_DOWNLOAD_DELAY"),"0.5"))
  stopifnot(is.finite(download_delay),download_delay>=0.5)
  timing_path <- file.path(out,"step_timings.csv")
  timed <- function(step, target, expr) {
    message("  Starting ",step,": ",target)
    began <- proc.time()[["elapsed"]]
    outcome <- "interrupted_or_error"
    on.exit({
      elapsed <- proc.time()[["elapsed"]]-began
      message(sprintf("  %s: %.2f sec (%s)",step,elapsed,outcome))
      entry <- data.table(RecordedAt=format(Sys.time(),"%Y-%m-%d %H:%M:%S"),
        Step=step,Target=target,ElapsedSeconds=round(elapsed,3),Outcome=outcome)
      fwrite(entry,timing_path,append=file.exists(timing_path))
    },add=TRUE)
    result <- force(expr)
    outcome <- "completed"
    result
  }
  cache_only <- Sys.getenv(paste0(env_prefix,"_CACHE_ONLY"),"0")=="1"
  retry_failed <- Sys.getenv(paste0(env_prefix,"_RETRY_FAILED"),"0")=="1"
  status_path <- file.path(out,"url_status.csv")
  url_status <- if(file.exists(status_path)) fread(status_path) else
    data.table(URL=character(),Status=character(),Message=character(),CheckedAt=character())
  if(!all(c("URL","Status","Message","CheckedAt") %in% names(url_status)))
    url_status <- data.table(URL=character(),Status=character(),Message=character(),CheckedAt=character())
  record_url_status <- function(url,status,message="") {
    row <- data.table(URL=url,Status=status,Message=substr(message,1L,500L),
      CheckedAt=format(Sys.time(),"%Y-%m-%d %H:%M:%S"))
    url_status <<- rbind(url_status[URL!=url],row)
    fwrite(url_status,status_path)
  }
  message(if(cache_only) "Cache-only run: no web downloads or download sleeps." else
    paste0("Missing pages: ",download_delay,"-second pause per request."))
  failed_urls <- new.env(parent=emptyenv())
  options(timeout=max(60,getOption("timeout")))
  cached_page <- function(url) {
    name <- paste0(gsub("[^A-Za-z0-9._-]","_",URLdecode(url)),".html")
    candidates <- c(file.path(cache,name),file.path(root,"EuropeanFootball/pipeline_data/Manual_Sources",
      c("Wikipedia_RSSSF_Four_Leagues/cache","Wikipedia_RSSSF_Remaining_UEFA_Leagues/cache"),name))
    any(file.exists(candidates))
  }
  fetch <- function(url, allow_download=TRUE) {
    name <- gsub("[^A-Za-z0-9._-]","_",URLdecode(url))
    path <- file.path(cache,paste0(name,".html"))
    # Reuse old raw pages if present; no bulk-download/extraction pipeline needed.
    old <- file.path(root,"EuropeanFootball/pipeline_data/Source/rsssf/all/pages",sub("https://www.rsssf.org/","",url,fixed=TRUE))
    if(grepl("^https://www.rsssf.org/",url) && file.exists(old)) {cache_reads <<- cache_reads+1L; return(read_html(old))}
    old_index <- file.path(root,"EuropeanFootball/pipeline_data/Source/rsssf/all/review/pages",basename(url))
    if(url %in% paste0("https://www.rsssf.org/",c("resultsp.html","resultsp99.html","resultsp00.html","resultsp2010.html","resultsp2020.html")) && file.exists(old_index)) {cache_reads <<- cache_reads+1L; return(read_html(old_index))}
    sibling_caches <- file.path(root,"EuropeanFootball/pipeline_data/Manual_Sources",
      c("Wikipedia_RSSSF_Four_Leagues/cache","Wikipedia_RSSSF_Remaining_UEFA_Leagues/cache"),
      paste0(name,".html"))
    sibling <- sibling_caches[file.exists(sibling_caches)]
    if(!file.exists(path) && length(sibling)) {cache_reads <<- cache_reads+1L; return(read_html(sibling[1L]))}
    prior <- url_status[URL==url]
    if(!file.exists(path) && !length(sibling) && nrow(prior) &&
       prior$Status[1L] %in% c("missing","failed") && !retry_failed) {
      stop("Cached unavailable page (set ",paste0(env_prefix,"_RETRY_FAILED=1")," to retry): ",url)
    }
    if(!file.exists(path)) {
      if(cache_only || !allow_download) stop("Page not cached (network disabled for this request): ",url)
      if(exists(url,envir=failed_urls,inherits=FALSE)) stop("Earlier download failed this run: ",url)
      assign(url,TRUE,envir=failed_urls)
      download_attempts <<- download_attempts+1L
      message("Attempting uncached download: ",url)
      Sys.sleep(download_delay); tmp <- paste0(path,".tmp"); on.exit(unlink(tmp),add=TRUE)
      notes <- character()
      status <- tryCatch(withCallingHandlers(download.file(url,tmp,method="libcurl",mode="wb",quiet=TRUE,
        headers=c("User-Agent"="J-Ratings historical research (Wikipedia fixtures; RSSSF dates)")),
        warning=function(w) {notes <<- c(notes,conditionMessage(w)); invokeRestart("muffleWarning")}),
        error=function(e) {notes <<- c(notes,conditionMessage(e)); 1L})
      if(status!=0 || file.info(tmp)$size==0) {
        record_url_status(url,if(any(grepl("404|410",notes))) "missing" else "failed",paste(notes,collapse="; "))
        stop("Download failed: ",url)
      }
      tryCatch(read_html(tmp),error=function(e) {
        record_url_status(url,"failed",conditionMessage(e)); stop(e)
      })
      if(!file.rename(tmp,path)) stop("Cannot save cache: ",path)
      record_url_status(url,"success","")
      rm(list=url,envir=failed_urls)
    } else cache_reads <<- cache_reads+1L
    read_html(path)
  }
  config <- data.table(Country=c("Poland","Norway","Sweden","Romania"),
    Prefix=c("tablesp/pol","tablesn/noo","tablesz/zwed","tablesr/roem"),
    League=c("Ekstraklasa","Eliteserien","Allsvenskan","SuperLiga"))
  if(!is.null(config_override)) config <- copy(config_override)
  countries <- trimws(strsplit(Sys.getenv(paste0(env_prefix,"_COUNTRIES"),paste(config$Country,collapse=",")),",",fixed=TRUE)[[1]])
  if(any(!countries %in% config$Country)) stop("Unknown country in ",env_prefix,"_COUNTRIES")
  config <- config[Country %in% countries]
  min_year <- as.integer(Sys.getenv(paste0(env_prefix,"_MIN_YEAR"),"1900"))
  default_max_year <- if(is.null(config_override)) "2025" else format(Sys.Date(),"%Y")
  max_year <- as.integer(Sys.getenv(paste0(env_prefix,"_MAX_YEAR"),default_max_year))
  stopifnot(!is.na(min_year),!is.na(max_year),min_year<=max_year)
  inventory <- list(); errors <- list()
  indexes <- paste0("https://www.rsssf.org/",c("resultsp.html","resultsp99.html","resultsp00.html","resultsp2010.html","resultsp2020.html"))
  for(url in indexes) tryCatch({
    doc <- fetch(url); a <- xml_find_all(doc,"//a[@href]")
    inventory[[length(inventory)+1L]] <- data.table(URL=url_absolute(xml_attr(a,"href"),url),Label=clean_text(xml_text(a)))
  },error=function(e) {errors[[length(errors)+1L]] <<- data.table(URL=url,Error=conditionMessage(e))})
  inv <- unique(rbindlist(inventory,fill=TRUE))
  if(!nrow(inv)) stop("Could not read any RSSSF season indexes; check network access")
  fwrite(inv,file.path(out,"discovered_links.csv"))
  audits <- list(); all_games <- list(); alias_reviews <- list()
  aliases_path <- file.path(out,"team_aliases.csv")
  if(!file.exists(aliases_path)) fwrite(data.table(Country=character(),RSSSF=character(),Wikipedia=character(),
    StartYear=integer(),EndYear=integer(),Reason=character(),Approved=logical()),aliases_path)
  aliases <- fread(aliases_path)
  # Older alias files had only Country/RSSSF/Wikipedia. Keep them valid while
  # allowing season-scoped approvals in the expanded format.
  for(nm in c("Country","RSSSF","Wikipedia")) if(!nm %in% names(aliases)) aliases[, (nm):=""]
  if(!"StartYear" %in% names(aliases)) aliases[,StartYear:=NA_integer_]
  if(!"EndYear" %in% names(aliases)) aliases[,EndYear:=NA_integer_]
  if(!"Reason" %in% names(aliases)) aliases[,Reason:=""]
  if(!"Approved" %in% names(aliases)) aliases[,Approved:=TRUE]
  aliases[,StartYear:=suppressWarnings(as.integer(StartYear))]
  aliases[,EndYear:=suppressWarnings(as.integer(EndYear))]
  # A blank approval in the generated review template is not approval.
  aliases <- aliases[!is.na(Approved) & Approved == TRUE]
  # Recover completed season artifacts, including those from interrupted runs
  # predating resume support. Keep their audit rows in every subsequent report.
  if(nrow(saved_audit)) {
    saved_audit <- saved_audit[Country %in% countries &
      (is.na(StartYear) | (StartYear>=min_year & StartYear<=max_year))]
    for(si in seq_len(nrow(saved_audit))) {
      row <- saved_audit[si]
      locked <- use_locks && nrow(locked_audit)>0L && row$RSSSF %in% locked_audit$RSSSF
      if(!resume && !locked) next
      key <- paste(row$Country,gsub("/","-",row$Season),sep="_")
      folder <- if(locked) file.path(out,"locked_seasons",key) else file.path(out,key)
      paths <- file.path(folder,c("all_wikipedia_games.csv","rsssf_evidence.csv","team_map.csv"))
      if(!nzchar(row$Error) && all(file.exists(paths))) {
        all_games[[key]] <- fread(paths[1])
        alias_reviews[[key]] <- alias_review_rows(row$Country,row$Season,row$StartYear,
          all_games[[key]],fread(paths[2]),fread(paths[3]))
        audits[[row$RSSSF]] <- row
      } else if(nzchar(row$Error) && !retry_failed) audits[[row$RSSSF]] <- row
    }
    message("Resuming: ",length(audits)," previously processed seasons retained.")
  }
  # Also flush partial reports on Esc/Ctrl+C. Per-season files are written first.
  on.exit({
    if(length(audits)) fwrite(rbindlist(audits,fill=TRUE),previous_audit)
    if(length(alias_reviews)) fwrite(rbindlist(alias_reviews,fill=TRUE),file.path(out,"alias_review.csv"))
  },add=TRUE)
  # User-reviewed aliases remain optional; no built-in club-specific exceptions.
  for(ci in seq_len(nrow(config))) {
    cfg <- config[ci]; country <- cfg$Country
    prefixes <- cfg$Prefix
    if("OldPrefixes" %in% colnames(cfg) && !is.na(cfg$OldPrefixes) && nzchar(cfg$OldPrefixes))
      prefixes <- c(prefixes,strsplit(cfg$OldPrefixes,"|",fixed=TRUE)[[1]])
    pattern <- paste0("^https://www[.]rsssf[.]org/(?:",paste(prefixes,collapse="|"),")[0-9]{2,4}[.]html$")
    pages <- unique(inv[grepl(pattern,URL)],by="URL")
    # Newest completed seasons may not yet appear in the index.
    latest <- if("Calendar" %in% names(cfg)) max_year+as.integer(!cfg$Calendar) else if(country %in% c("Poland","Romania")) max_year+1L else max_year
    if(latest>=2020L && !cache_only) pages <- unique(rbind(pages,data.table(URL=paste0("https://www.rsssf.org/",cfg$Prefix,latest,".html"),Label="")),by="URL")
    pages[, PageYear:=as.integer(sub(".*?([0-9]{2,4})[.]html$","\\1",URL))]
    pages[PageYear<100,PageYear:=ifelse(PageYear<=26L,PageYear+2000L,PageYear+1900L)]
    # Distinguish e.g. Romania roem24 (1923/24) from modern roem2024.
    for(pi in seq_len(nrow(pages))) {
      lm <- capture("([12][0-9]{3})(?:[/\u2013-]([0-9]{2,4}))?",pages$Label[pi])
      if(length(lm)) {
        y <- as.integer(lm[2]); if(nzchar(lm[3])) y <- y+1L
        pages$PageYear[pi] <- y
      }
    }
    pages <- pages[PageYear>=min_year & PageYear<=max_year+1L][order(-PageYear)]
    for(url in pages$URL) {
      if(url %in% names(audits)) next
      season <- NA_character_; start <- NA_integer_; wiki_url <- NA_character_
      message(country,": ",url)
      tryCatch({
        doc <- timed("RSSSF page read",url,fetch(url))
        title <- xml_text(xml_find_first(doc,"//h1 | //h2 | //title"))
        sm <- capture("([12][0-9]{3})(?:[/\u2013-]([0-9]{2,4}))?",title)
        if(!length(sm)) stop("Cannot establish season from RSSSF title")
        start <- as.integer(sm[2]); end <- start
        if(nzchar(sm[3])) { end <- as.integer(sm[3]); if(end<100) end <- (start%/%100)*100+end; if(end<start) end <- end+100 }
        if(start<min_year || start>max_year) next
        season <- if(start==end) as.character(start) else sprintf("%d/%02d",start,end%%100)
        label <- if(start==end) as.character(start) else sprintf("%d\u2013%02d",start,end%%100)
        names <- switch(country,Poland=c("Ekstraklasa","I liga"),Norway=c("Eliteserien","Tippeligaen","1. divisjon","Hovedserien","Norgesserien"),Sweden="Allsvenskan",Romania=c("Liga I","Divizia A","Romanian football championship"))
        if("WikiNames" %in% colnames(cfg)) names <- strsplit(cfg$WikiNames,"|",fixed=TRUE)[[1]]
        wg <- NULL; failures <- character()
        wiki_urls <- vapply(names,function(nm) paste0("https://en.wikipedia.org/wiki/",
          URLencode(gsub(" ","_",paste(label,nm)),reserved=TRUE)),character(1))
        # Use the known working title before trying speculative modern names.
        if(nrow(saved_audit)) wiki_urls <- unique(c(saved_audit[Country==country & Season==season, Wikipedia],wiki_urls))
        wiki_urls <- wiki_urls[!is.na(wiki_urls) & nzchar(wiki_urls)]
        wiki_urls <- wiki_urls[order(!vapply(wiki_urls,cached_page,logical(1)))]
        for(u in wiki_urls) {
          parsed <- tryCatch({
            wiki_doc <- timed("Wikipedia page read",u,fetch(u,allow_download=start>=wiki_download_min))
            timed("Wikipedia table parsing",u,wiki_games(wiki_doc))
          },error=function(e) {failures <<- c(failures,paste(u,conditionMessage(e))); NULL})
          if(!is.null(parsed) && nrow(parsed$games)) {wg <- parsed; wiki_url <- u; break}
        }
        if(is.null(wg)) stop("No supported Wikipedia results matrix: ",paste(failures,collapse="; "))
        rg <- timed("RSSSF fixture parsing",url,rsssf_games(doc,start,end))
        message("  Parsed ",nrow(wg$games)," Wikipedia games; ",nrow(rg),
          " RSSSF rows; ",length(unique(c(rg$RHome,rg$RAway)))," RSSSF team names.")
        matched <- timed("Team and fixture matching",paste(country,season),
          attach_dates(wg$games,rg,aliases[Country==country],start))
        games <- matched$games
        games[, `:=`(Country=country,Season=season,Competition=cfg$League,CompetitionType="league",Tier=1L,
          League=cfg$League,Source="wikipedia",SourcePage=wiki_url,DateSource="RSSSF",DateSourcePage=url,DateApprox=FALSE)]
        games[, Result:=vapply(strsplit(Score,"-",fixed=TRUE),function(s) {v<-as.integer(s); if(v[1]>v[2]) "1-0" else if(v[1]<v[2]) "0-1" else "0.5-0.5"},character(1))]
        key <- paste(country,gsub("/","-",season),sep="_")
        season_dir <- file.path(out,key); dir.create(season_dir,showWarnings=FALSE)
        fwrite(games,file.path(season_dir,"all_wikipedia_games.csv"))
        fwrite(games[DateStatus!="matched"],file.path(season_dir,"unresolved.csv"))
        fwrite(matched$mapping,file.path(season_dir,"team_map.csv"))
        if(!is.null(matched$candidates) && nrow(matched$candidates)) fwrite(matched$candidates,file.path(season_dir,"team_match_candidates.csv"))
        fwrite(rg,file.path(season_dir,"rsssf_evidence.csv"))
        fwrite(wg$tables,file.path(season_dir,"wikipedia_tables.csv"))
        all_games[[key]] <- games
        alias_reviews[[key]] <- alias_review_rows(country,season,start,games,rg,matched$mapping)
        audits[[url]] <- data.table(Country=country,Season=season,StartYear=start,
          WikipediaGames=nrow(games),DatedGames=sum(games$DateStatus=="matched"),
          WikipediaMatrixGames=sum(!is.na(games$WikiTable)),
          ExtractionReview=if(!nrow(rg)) "No RSSSF fixtures extracted; inspect source layout/date availability" else if(!any(!is.na(rg$Date))) "RSSSF results extracted without dates; inspect date format/availability" else if(!any(!is.na(games$WikiTable))) "No results matrix extracted; only individual reports" else
            if(nrow(rg)>0 && nrow(games)<0.8*nrow(rg)) "Wikipedia count below 80% of RSSSF count; inspect missing stages or extra RSSSF competitions" else "",
          RSSSFResults=nrow(rg),RSSSFDatedResults=sum(!is.na(rg$Date)),
          SourceAssessment=attr(rg,"source_assessment"),
          UnresolvedTeamNames=sum(is.na(matched$mapping$Wikipedia)),
          Status=if(all(games$DateStatus=="matched")) "all_extracted_games_dated" else "partial_dates",
          RSSSF=url,Wikipedia=wiki_url,Error="")
      },error=function(e) {
        audits[[url]] <<- data.table(Country=country,Season=season,StartYear=start,
          WikipediaGames=0L,DatedGames=0L,Status="needs_review",RSSSF=url,Wikipedia=wiki_url,Error=conditionMessage(e))
      })
      audit_now <- rbindlist(audits,fill=TRUE)
      audit_now[, CoveragePercent:=ifelse(WikipediaGames>0,round(100*DatedGames/WikipediaGames,2),NA_real_)]
      audit_now[, Meets95Percent:=ifelse(is.na(CoveragePercent),NA,DatedGames/WikipediaGames>=0.95)]
      audits[[url]] <- audit_now[RSSSF==url]
      if(use_locks && freeze_season(out,audits[[url]])) {
        message("  Season protected: ",country," ",season)
      }
      fwrite(audit_now,file.path(out,"season_audit.csv"))
      print(tail(audit_now[,.(Country,Season,WikipediaGames,DatedGames,CoveragePercent,Status)],1L))
    }
  }
  combined <- rbindlist(all_games,fill=TRUE)
  if(use_locks) {
    locks <- read_season_locks(out)
    if(nrow(locks)) fwrite(locks,file.path(out,"locked_seasons.csv"))
  }
  audit_now <- rbindlist(audits,fill=TRUE)
  if(nrow(combined)) {
    fwrite(combined,file.path(out,"all_wikipedia_games.csv"))
    fwrite(combined[DateStatus=="matched"],file.path(out,"dated_candidate.csv"))
    fwrite(combined[DateStatus!="matched"],file.path(out,"unresolved.csv"))
    fwrite(combined[,.(WikipediaGames=.N,DatedGames=sum(DateStatus=="matched"),
      EarliestDatedSeason=if(any(DateStatus=="matched")) min(Season[DateStatus=="matched"]) else NA_character_),by=Country],file.path(out,"coverage_summary.csv"))
    if(length(alias_reviews)) fwrite(rbindlist(alias_reviews,fill=TRUE),file.path(out,"alias_review.csv"))
  } else {
    # Do not leave a previous run's candidate looking like this run's result.
    for(name in c("all_wikipedia_games.csv","dated_candidate.csv","unresolved.csv")) fwrite(empty_games(),file.path(out,name))
  }
  if(length(errors)) fwrite(rbindlist(errors),file.path(out,"index_errors.csv"))
  season_errors <- if(length(audits)) sum(nzchar(rbindlist(audits,fill=TRUE)$Error)) else 0L
  message("Local page reads: ",cache_reads,"; download attempts for uncached pages: ",download_attempts,".")
  message("Finished with ",season_errors," season errors. Review ",file.path(out,"season_audit.csv"),". Production master unchanged.")
  if(season_errors>0L) warning("Some seasons failed; inspect the Error column before interpreting coverage.",call.=FALSE)
  if(length(audits)) {
    summary <- audit_now[,.(Seasons=.N,SeasonsWithErrors=sum(nzchar(Error)),
      SeasonsAtLeast95=sum(Meets95Percent,na.rm=TRUE),
      WikipediaGames=sum(WikipediaGames),DatedGames=sum(DatedGames)),by=Country]
    summary[,CoveragePercent:=ifelse(WikipediaGames>0,round(100*DatedGames/WikipediaGames,2),NA_real_)]
    fwrite(summary,file.path(out,"coverage_summary.csv"))
    print(summary)
  }
  invisible(combined)
}

if(Sys.getenv("FOUR_LEAGUES_FUNCTIONS_ONLY")!="1") {
  run_four_leagues()
  if (requireNamespace("beepr", quietly = TRUE)) beepr::beep() else warning("Install beepr to hear the completion beep.")
}
