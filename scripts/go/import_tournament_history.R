# One-off, resumable historical import. Run from the repository root.
# No Elo calculation, production game import, Git operations or workflow changes.
# Live requests require GO_K2SS_PERMISSION_GRANTED=true.
go_import_tournament_history <- function(download=utils::download.file, wait=Sys.sleep) {
  offline <- identical(tolower(Sys.getenv("GO_HISTORY_OFFLINE", "false")), "true")
  if (!offline && tolower(Sys.getenv("GO_K2SS_PERMISSION_GRANTED", "false")) != "true") {
    cat("Permission pending: no requests or files changed. Set the permission flag only after the owner agrees to historical reuse.\n")
    return(invisible(NULL))
  }
  suppressPackageStartupMessages({library(data.table); library(jsonlite); library(xml2)})
  root <- "Go/pipeline_data/tournament_history"
  cache <- "Go/pipeline_data/tournament_format_audit/cache"
  dir.create(root, recursive=TRUE, showWarnings=FALSE)
  dir.create(cache, recursive=TRUE, showWarnings=FALSE)
  state_path <- file.path(root, "progress.rds")
  state <- if (file.exists(state_path)) readRDS(state_path) else
    list(schedule_queue="/schedules/", schedules=character(), months=character(),
         edition_queue=character(), editions=character(), built=character())
  save <- function() {
    tmp <- paste0(state_path, ".part"); saveRDS(state, tmp)
    # Keep the last checkpoint recoverable even on Windows.
    backup <- paste0(state_path, ".previous")
    if (file.exists(backup)) unlink(backup)
    if (file.exists(state_path) && !file.rename(state_path, backup)) stop("Cannot preserve checkpoint")
    if (!file.rename(tmp, state_path)) {
      if (file.exists(backup)) file.rename(backup, state_path)
      stop("Cannot save checkpoint")
    }
  }
  last_finished <- NULL; requests <- 0L; missing <- character()
  consecutive_server_errors <- 0L
  fetch <- function(link, path, json=FALSE) {
    if (file.exists(path)) return(TRUE)
    if (link %in% state$unavailable) {missing <<- unique(c(missing,link)); return(FALSE)}
    if (offline) {missing <<- unique(c(missing, link)); return(FALSE)}
    if (!is.null(last_finished)) {
      pause <- 3-as.numeric(difftime(Sys.time(), last_finished, units="secs"))
      if (pause > 0) wait(pause)
    }
    tmp <- paste0(path, ".part")
    on.exit({last_finished <<- Sys.time(); if (file.exists(tmp)) unlink(tmp)}, add=TRUE)
    old_timeout <- getOption("timeout"); options(timeout=60)
    on.exit(options(timeout=old_timeout), add=TRUE)
    requests <<- requests+1L
    http_status <- NA_integer_
    status <- tryCatch(withCallingHandlers(download(paste0("https://gotoeveryone.k2ss.info", link), tmp,
                            method="libcurl", mode="wb", quiet=TRUE), warning=function(w) {
      hit <- regmatches(conditionMessage(w),regexpr("HTTP status was '[0-9]{3}",conditionMessage(w)))
      if (length(hit) && nzchar(hit)) {
        http_status <<- as.integer(sub(".*'","",hit))
        if (http_status==404L || http_status>=500L) invokeRestart("muffleWarning")
      }
    }), error=function(e) {
      if (!is.na(http_status) && (http_status==404L || http_status>=500L)) return(http_status)
      stop("Request failed; no retry. Rerun to resume: ",link," | ",conditionMessage(e))
    })
    if (identical(status,404L)) {
      consecutive_server_errors <<- 0L
      state$unavailable <<- unique(c(state$unavailable,link)); save()
      missing <<- unique(c(missing,link)); return(FALSE)
    }
    if (status>=500L && status<=599L) {
      consecutive_server_errors <<- consecutive_server_errors+1L
      state$server_errors[[link]] <<- list(status=status,lastAttempt=as.character(Sys.time()))
      save()
      write_json(state$server_errors,file.path(root,"deferred-server-errors.json"),auto_unbox=TRUE,pretty=TRUE)
      missing <<- unique(c(missing,link))
      cat("Deferred HTTP",status,":",link,"(no retry this run)\n"); flush.console()
      if (consecutive_server_errors>=3L) stop("Three consecutive server errors; paused to avoid requesting more pages from an unhealthy site. Progress saved.")
      return(FALSE)
    }
    if (status != 0L) stop("Request failed; no retry. Rerun to resume: ", link)
    consecutive_server_errors <<- 0L
    state$server_errors[[link]] <<- NULL
    if (json) {
      days <- fromJSON(tmp, simplifyVector=FALSE)
      if (!is.list(days) || (length(days) && !all(vapply(days, function(d)
        is.list(d) && !is.null(d$date), TRUE)))) stop("Unexpected calendar response: ", link)
    } else read_html(tmp)
    if (!file.rename(tmp, path)) stop("Cannot save cache: ", path)
    TRUE
  }
  hrefs <- function(path) {
    links <- xml_attr(xml_find_all(read_html(path), "//a[@href]"), "href")
    links <- sub("^https://gotoeveryone.k2ss.info", "", links)
    unique(sub("[?#].*$", "", links[!is.na(links)]))
  }
  calendar_path <- file.path(root, "calendar_events.csv")
  original <- "Go/pipeline_data/tournament_format_audit/calendar_events.csv"
  events <- if (file.exists(calendar_path)) fread(calendar_path) else if (file.exists(original)) fread(original) else
    data.table(Date=character(), Label=character(), Link=character(), Family=character())
  events[, Date:=as.character(Date)]
  events[, Family:=sub("^(/news/[^/]+/[^/]+/).*", "\\1", Link)]
  # Follow only advertised archive years/months. Never infer an earliest year
  # from a failed request or probe backwards through arbitrary years.
  while (length(todo <- setdiff(state$schedule_queue, state$schedules))) {
    link <- todo[1L]
    path <- file.path(cache, paste0("history", gsub("[^a-zA-Z0-9]+", "_", link), ".html"))
    if (!fetch(link, path)) break
    links <- hrefs(path)
    archives <- links[grepl("^/schedules/([0-9]{4}/([0-9]{1,2}/)?)?$", links)]
    state$schedule_queue <- unique(c(state$schedule_queue, archives))
    years <- unique(sub("^/schedules/([0-9]{4})/.*", "\\1", archives[grepl("^/schedules/[0-9]{4}/", archives)]))
    # Every month in each explicitly advertised year; missing months are
    # reported rather than silently treated as the end of history.
    discovered <- unlist(lapply(years, function(y) sprintf("%s/%02d", y, 1:12)))
    discovered <- discovered[discovered <= format(Sys.Date(),"%Y/%m")]
    state$month_queue <- unique(c(state$month_queue, discovered,
      sub("\\.json$", "", sub("^/json/schedules/", "", links[grepl("^/json/schedules/[0-9]{4}/[0-9]{2}\\.json$", links)]))))
    state$schedules <- unique(c(state$schedules, link)); save()
    cat("Archive navigation:", link, "\n"); flush.console()
  }
  # Optional explicit range when archive links are rendered only by JavaScript.
  # Both bounds must be supplied; this is never silently assumed to be complete.
  start <- Sys.getenv("GO_HISTORY_START_MONTH", ""); end <- Sys.getenv("GO_HISTORY_END_MONTH", "")
  if (nzchar(start) || nzchar(end)) {
    stopifnot(grepl("^[0-9]{4}-[0-9]{2}$", start), grepl("^[0-9]{4}-[0-9]{2}$", end))
    state$month_queue <- unique(c(state$month_queue, format(seq(as.Date(paste0(start,"-01")),
      as.Date(paste0(end,"-01")), by="month"), "%Y/%m")))
  }
  # Previously saved calendars are useful even when working entirely offline.
  saved_months <- list.files(cache, pattern="^[0-9]{4}_[0-9]{2}\\.json$")
  state$month_queue <- sort(unique(c(state$month_queue, gsub("_", "/", sub("\\.json$", "", saved_months)))))
  if (!length(state$month_queue)) stop("No archive months discovered. Inspect saved timetable HTML; supply verified GO_HISTORY_START_MONTH and GO_HISTORY_END_MONTH if necessary.")
  for (month in setdiff(state$month_queue, state$months)) {
    path <- file.path(cache, paste0(gsub("/", "_", month), ".json"))
    if (!fetch(paste0("/json/schedules/", month, ".json"), path, TRUE)) next
    days <- fromJSON(path, simplifyVector=FALSE); rows <- list()
    for (day in days) for (item in day$items) {
      if (!is.null(item$link) && grepl("^/news/[a-z-]+/[a-z0-9-]+/[a-z0-9-]+/$", item$link))
        rows[[length(rows)+1L]] <- data.table(Date=day$date, Label=item$name, Link=item$link,
          Family=sub("^(/news/[^/]+/[^/]+/).*", "\\1", item$link))
    }
    events <- events[!startsWith(Date, gsub("/", "-", month))]
    if (length(rows)) events <- unique(rbind(events, rbindlist(rows), fill=TRUE))
    fwrite(events, calendar_path)
    state$months <- unique(c(state$months, month)); save()
    cat("Calendar:", month, "\n"); flush.console()
  }
  state$edition_queue <- unique(c(state$edition_queue, events$Link)); save()
  attempted <- character()
  while (length(todo <- setdiff(state$edition_queue, c(state$editions,attempted)))) {
    link <- todo[1L]
    path <- file.path(cache, paste0(gsub("[^a-zA-Z0-9]+", "_", link), ".html"))
    attempted <- c(attempted,link)
    if (!fetch(link, path)) next
    # Follow actual edition navigation within the same family, not invented IDs.
    family <- sub("^(/news/[^/]+/[^/]+/).*", "\\1", link)
    doc <- read_html(path)
    links <- xml_attr(xml_find_all(doc, "//nav[contains(@class,'select-holding')]//a[@href]"), "href")
    links <- sub("^https://gotoeveryone.k2ss.info", "", links)
    links <- links[!is.na(links) & startsWith(links, family) & grepl("^/news/[a-z-]+/[a-z0-9-]+/[a-z0-9-]+/$", links)]
    state$edition_queue <- unique(c(state$edition_queue, links))
    state$editions <- unique(c(state$editions, link)); save()
    cat("Saved editions:", length(state$editions), "/", length(state$edition_queue), link, "\n"); flush.console()
  }
  # Reuse the daily parser in offline mode, in checkpointed batches. No second
  # network pass, historical date filters, player scraping or Elo calculation.
  vars <- c("GO_TOURNAMENT_FUNCTIONS_ONLY", "GO_TOURNAMENT_OFFLINE", "GO_TOURNAMENT_LINKS",
    "GO_TOURNAMENT_MAX_PAGES", "GO_TOURNAMENT_CALENDAR", "GO_TOURNAMENT_STATE_DIR", "GO_TOURNAMENT_SKIP_LINK_BUILD", "GO_TOURNAMENT_LINK_OUTPUT")
  old <- Sys.getenv(vars, unset=NA_character_)
  on.exit(for (v in vars) if (is.na(old[[v]])) Sys.unsetenv(v) else do.call(Sys.setenv, setNames(list(old[[v]]),v)), add=TRUE)
  Sys.setenv(GO_TOURNAMENT_FUNCTIONS_ONLY="true", GO_TOURNAMENT_OFFLINE="true",
    GO_TOURNAMENT_MAX_PAGES="25", GO_TOURNAMENT_SKIP_LINK_BUILD="true", GO_TOURNAMENT_CALENDAR=calendar_path,
    GO_TOURNAMENT_STATE_DIR=file.path(root,"build"))
  source("scripts/go/update_tournaments.R")
  available <- state$edition_queue[vapply(state$edition_queue, function(link)
    file.exists(file.path(cache,paste0(gsub("[^a-zA-Z0-9]+","_",link),".html"))), TRUE)]
  todo <- setdiff(available, state$built)
  changed <- FALSE
  for (batch in split(todo, ceiling(seq_along(todo)/25))) {
    Sys.setenv(GO_TOURNAMENT_LINKS=paste(batch,collapse=","))
    report <- go_update_tournaments()
    changed <- changed || length(report$updated)>0L
    write_json(report, file.path(root,paste0("batch-",length(state$built),"-report.json")), auto_unbox=TRUE, null="null", pretty=TRUE)
    state$built <- unique(c(state$built,batch)); save()
  }
  if(changed) {
    Sys.setenv(GO_TOURNAMENT_LINK_OUTPUT=Sys.getenv("GO_TOURNAMENT_OUTPUT","Go/data/tournaments/catalogue"))
    source("scripts/go/build_tournament_links.R")
  }
  pending_months <- setdiff(state$month_queue,state$months)
  fwrite(data.table(Link=missing),file.path(root,"missing-cache.csv"))
  write_json(list(requests=requests, calendarMonths=length(state$months),
    firstCalendar=if(length(state$months))min(state$months) else NULL,
    lastCalendar=if(length(state$months))max(state$months) else NULL,
    pendingMonths=as.list(pending_months), deferredServerErrors=state$server_errors, unavailableURLs=as.list(state$unavailable), pendingArchives=as.list(setdiff(state$schedule_queue,state$schedules)),
    editionsDiscovered=length(state$edition_queue), editionsProcessed=length(state$built),
    note="Processed includes unsupported editions; inspect batch review reports. Archive completeness requires checking the discovered span against the source timetable."),
    file.path(root,"summary.json"),auto_unbox=TRUE,null="null",pretty=TRUE)
  cat("Historical import finished:",length(state$built),"editions processed;",requests,
      "requests this run. Review",root,"before publication. No Git or Elo changes.\n")
}
if (!identical(Sys.getenv("GO_HISTORY_FUNCTIONS_ONLY"), "true")) go_import_tournament_history()
