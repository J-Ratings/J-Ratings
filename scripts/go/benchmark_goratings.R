# Run from R: source("scripts/go/benchmark_goratings.R")
# Set options(goratings.benchmark.mode = "daily") to measure daily CSV saves,
# or "full" to measure per-profile RDS checkpoints. Default: weekly.
benchmark_goratings <- function(root = getwd(),
                                mode = getOption("goratings.benchmark.mode", "weekly")) {
  mode <- match.arg(mode, c("weekly", "daily", "full"))
  packages <- c("rvest", "dplyr", "readr", "stringr", "tibble", "purrr", "curl")
  for (package in packages) {
    if (!requireNamespace(package, quietly = TRUE)) stop("Missing R package: ", package)
  }
  # Load the production helper definitions, without running its top-level job.
  scraper <- new.env(parent = baseenv())
  for (package in packages) {
    exports <- getNamespaceExports(package)
    for (name in exports) {
      if (!exists(name, scraper, inherits = FALSE))
        assign(name, getExportedValue(package, name), scraper)
    }
  }
  definitions <- parse(file.path(root, "scripts/go/04_scrape_goratings.R"), encoding = "UTF-8")
  locked_body <- Filter(function(expr)
    is.call(expr) && identical(expr[[1]], as.name("go_with_goratings_lock")), definitions)
  stopifnot(length(locked_body) == 1L)
  definitions <- as.list(locked_body[[1]][[2]])[-1L]
  helpers <- c("LIVE_START_DATE", "REQUEST_DELAY_MIN", "REQUEST_DELAY_MAX",
               "clean_text", "clean_id", "safe_integer", "empty_observations",
               "read_existing_observations", "observation_key",
               "deduplicate_observations", "parse_player_profile")
  for (expr in definitions) {
    if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
        is.symbol(expr[[2]]) && as.character(expr[[2]]) %in% helpers)
      eval(expr, scraper)
  }
  stopifnot(all(vapply(helpers, exists, logical(1), envir = scraper, inherits = FALSE)))

  input <- file.path(root, "Go/pipeline_data/goratings")
  out <- tempfile("goratings-top10-")
  if (!dir.create(out)) stop("Could not create benchmark output directory")
  report <- file.path(out, "timings.csv")
  cat("Mode:", mode, "\nTimings:", report, "\n")
  rows <- list()
  rank <- NA_integer_
  player <- "run setup"
  player_id <- NA_character_
  add <- function(stage, elapsed, cpu = NA_real_, value = NA_real_, detail = "") {
    rows[[length(rows) + 1L]] <<- data.frame(
      rank = rank, player_id = player_id, player = player, stage = stage,
      seconds = elapsed, cpu_seconds = cpu, value = value, detail = detail)
  }
  measure <- function(stage, expr) {
    start <- proc.time()
    on.exit({
      delta <- proc.time() - start
      add(stage, unname(delta["elapsed"]), sum(delta[c("user.self", "sys.self")]))
    })
    force(expr)
  }
  save_report <- function() readr::write_csv(dplyr::bind_rows(rows), report)
  on.exit(if (length(rows)) save_report(), add = TRUE)

  top <- measure("read_original_top10_from_cache", {
    players <- readr::read_csv(file.path(input, "goratings_players_homepage.csv"),
                               col_types = readr::cols(player_id = readr::col_character()),
                               show_col_types = FALSE)
    ids <- c("1313", "1399", "1511", "1746", "1195", "1423", "1135", "1253", "1363", "1090")
    players[match(ids, players$player_id), ]
  })
  if (nrow(top) != 10L || anyNA(top$profile_url) ||
      !all(grepl("^https://www\\.goratings\\.org/en/players/[0-9]+\\.html$", top$profile_url)))
    stop("Expected 10 valid GoRatings profile URLs in the cached homepage.")
  existing <- measure("read_existing_observations",
    scraper$read_existing_observations(file.path(input, "goratings_games_2026_observations.csv")))
  daily <- existing
  failures <- tibble::tibble(player_id = character(), player_name = character(),
                             profile_url = character(), error = character(), failed_at = character())
  parsed <- vector("list", nrow(top))
  comparisons <- vector("list", nrow(top))
  last_finished <- NULL
  for (i in seq_len(nrow(top))) {
    rank <- top$homepage_rank[[i]]
    player <- top$player_name[[i]]
    player_id <- top$player_id[[i]]
    url <- top$profile_url[[i]]
    cat(sprintf("\n[%d/10] %s (%s)\n", i, player, player_id))
    first <- length(rows) + 1L
    profile_started <- proc.time()[["elapsed"]]
    measure("pacing_sleep", {
      if (!is.null(last_finished)) {
        gap <- if (mode == "daily") 3 else stats::runif(1, scraper$REQUEST_DELAY_MIN, scraper$REQUEST_DELAY_MAX)
        Sys.sleep(max(0, gap - as.numeric(difftime(Sys.time(), last_finished, units = "secs"))))
      }
    })
    read_profile <- function(url) {
      # Same libcurl GET and XML parser as read_html(URL), with an explicit
      # response buffer so transfer and DOM parsing can be measured separately.
      response <- measure("http_request", curl::curl_fetch_memory(url))
      add("http_response_bytes", NA_real_, value = length(response$content),
          detail = paste("HTTP", response$status_code))
      times <- response$times
      # These are cumulative libcurl milestones within http_request, not
      # extra durations. DNS can be zero on a cached lookup.
      for (name in names(times)) add(paste0("curl_cumulative_", name), unname(times[[name]]))
      if (response$status_code >= 400L) stop("HTTP ", response$status_code)
      measure("html_dom_parse", rvest::read_html(response$content, base_url = url))
    }
    failure <- NULL
    parsed[[i]] <- tryCatch({
      dom <- read_profile(url)
      times <- numeric()
      results <- list()
      # Alternate which parser runs first so warm-up does not consistently
      # favour either implementation.
      methods <- c("legacy", "date_first")
      if (i %% 2L == 0L) methods <- rev(methods)
      for (method in methods) {
        started <- proc.time()[["elapsed"]]
        results[[method]] <- measure(paste0(method, "_parser_total"),
          scraper$parse_player_profile(url, player_id, player,
            read_profile = function(url) dom,
            measure = function(stage, expr) measure(paste0(method, "_", stage), expr),
            date_first = method == "date_first"))
        times[[method]] <- proc.time()[["elapsed"]] - started
      }
      legacy <- results[["legacy"]]
      fast <- results[["date_first"]]
      identical_games <- identical(legacy[setdiff(names(legacy), "scraped_at")],
                                   fast[setdiff(names(fast), "scraped_at")])
      comparisons[[i]] <- data.frame(player_id = player_id, player = player,
        legacy_seconds = times[["legacy"]], date_first_seconds = times[["date_first"]],
        legacy_games = nrow(legacy), date_first_games = nrow(fast), identical_games = identical_games)
      add("parser_output_match", NA_real_, value = as.integer(identical_games))
      cat("  Same games and fields:", if (identical_games) "YES" else "NO", "\n")
      legacy
    },
      error = function(e) {
        failure <<- conditionMessage(e)
        scraper$empty_observations()
      })
    last_finished <- Sys.time()
    if (is.null(failure) && mode == "daily" && !nrow(parsed[[i]]))
      failure <- "No live games parsed from a daily profile."
    if (!is.null(failure)) {
      add("profile_error", NA_real_, detail = failure)
      failures <- dplyr::bind_rows(failures, tibble::tibble(
        player_id = player_id, player_name = player, profile_url = url, error = failure,
        failed_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")))
      cat("  Error:", failure, "\n")
    } else if (mode == "daily") {
      joined <- measure("daily_bind", dplyr::bind_rows(daily, parsed[[i]]))
      daily <- measure("daily_deduplicate", scraper$deduplicate_observations(joined))
      measure("daily_write_observations", readr::write_csv(daily, file.path(out, "observations.csv")))
      measure("daily_write_failures", readr::write_csv(failures, file.path(out, "failures.csv")))
    } else if (mode == "full") {
      measure("full_write_checkpoint", {
        path <- file.path(out, paste0(i, ".rds"))
        tmp <- paste0(path, ".tmp")
        saveRDS(list(id = player_id, observations = parsed[[i]]), tmp, compress = FALSE)
        if (!file.rename(tmp, path)) stop("Cannot save temporary checkpoint: ", path)
      })
    }
    add("games_retained", NA_real_, value = nrow(parsed[[i]]))
    add("benchmark_total_including_both_parsers", proc.time()[["elapsed"]] - profile_started)
    current <- dplyr::bind_rows(rows[first:length(rows)])
    visible <- is.finite(current$seconds) & !startsWith(current$stage, "curl_cumulative_")
    cat(paste(sprintf("  %-42s %8.3f s", current$stage[visible], current$seconds[visible]),
              collapse = "\n"), "\n")
    curl_rows <- current[startsWith(current$stage, "curl_cumulative_"), ]
    if (nrow(curl_rows)) cat("  HTTP milestones (seconds since request start):",
      paste(sprintf("%s=%.3f", sub("^curl_cumulative_", "", curl_rows$stage),
                    curl_rows$seconds), collapse = ", "), "\n")
    cat("  Games retained:", nrow(parsed[[i]]), "\n")
    save_report()
  }
  rank <- NA_integer_
  player <- "end of run"
  player_id <- NA_character_
  new <- measure("final_bind_profiles", dplyr::bind_rows(parsed))
  joined <- measure("final_bind_existing", dplyr::bind_rows(existing, new))
  all <- measure("final_deduplicate", scraper$deduplicate_observations(joined))
  measure("final_write_observations", readr::write_csv(all, file.path(out, "observations.csv")))
  measure("final_write_failures", readr::write_csv(failures, file.path(out, "failures.csv")))
  timings <- dplyr::bind_rows(rows)
  comparison <- dplyr::bind_rows(comparisons)
  if (nrow(comparison)) {
    comparison$speedup <- with(comparison, ifelse(date_first_seconds > 0,
                                                 legacy_seconds / date_first_seconds, NA_real_))
    readr::write_csv(comparison, file.path(out, "comparison.csv"))
    cat("\nPaired parser comparison (excludes download, HTML parsing and pacing):\n")
    print(comparison, row.names = FALSE)
    matched <- comparison[comparison$identical_games, ]
    if (nrow(matched)) cat(sprintf(
      "Matched profiles: %d/10; total parser time %.2f s -> %.2f s\n",
      nrow(matched), sum(matched$legacy_seconds), sum(matched$date_first_seconds)))
    cat("Comparison CSV:", file.path(out, "comparison.csv"), "\n")
  }
  cat("\nEnd-of-run timings:\n")
  print(timings[is.na(timings$rank), c("stage", "seconds", "cpu_seconds")], row.names = FALSE)
  cat("\nBenchmark complete. Timing CSV:", report, "\n")
  invisible(timings)
}

source("scripts/go/goratings_scrape_lock.R", local = TRUE)
go_with_goratings_lock(benchmark_goratings())
