# Stage 1: discover and cache RSSSF pages for missing top-flight seasons (2010+)
# plus Wales tier 2 (Cymru North/Cymru South and predecessors).  No master changes.
suppressPackageStartupMessages({ library(data.table); library(xml2) })
root <- normalizePath(Sys.getenv("J_RATINGS_REPO", getwd()), winslash = "/", mustWork = TRUE)
base <- file.path(root, "EuropeanFootball/pipeline_data")
out <- file.path(base, "Manual_Sources/RSSSF_Gap_Recovery")
pages <- file.path(out, "pages"); dir.create(pages, recursive=TRUE, showWarnings=FALSE)
min_year <- as.integer(Sys.getenv("GAP_RECOVERY_MIN_YEAR", "2010")); max_year <- as.integer(Sys.getenv("GAP_RECOVERY_MAX_YEAR", "2025"))
delay <- as.numeric(Sys.getenv("GAP_RECOVERY_DOWNLOAD_DELAY", "0.5"))
apply_download <- identical(Sys.getenv("DOWNLOAD_RSSSF_GAP_RECOVERY"), "1")
gaps <- fread(file.path(base, "Manual_Sources/Global/top_flight_span_overview/internal_gap_years.csv"))
gaps <- gaps[MissingYear >= min_year & MissingYear <= max_year, .(Country, StartYear=as.integer(MissingYear), Tier=1L, Scope="top_flight")]

# Archive pages contain the authoritative historic filename links; do not guess
# names such as bulg10 versus bulg2010.
archive_urls <- c("https://www.rsssf.org/resultsp2010.html", "https://www.rsssf.org/resultsp2020.html")
links <- rbindlist(lapply(archive_urls, function(u) {
  message("Reading RSSSF archive index: ", u)
  d <- read_html(u); a <- xml_find_all(d, "//a[contains(@href,'.html')]")
  data.table(Label=trimws(xml_text(a)), Href=xml_attr(a,"href"))
}), fill=TRUE)
links <- unique(links[nzchar(Label) & nzchar(Href) & !grepl("women|futsal|youth", Label, ignore.case=TRUE)])
aliases <- c("Hong Kong"="Hongkong", "DR Congo"="Congo-Kinshasa", "Congo"="Congo-Brazzaville", "United States Virgin Islands"="US Virgin Islands", "Suriname"="Surinam")
find_link <- function(country, year) {
  alternate <- if (country %chin% names(aliases)) unname(aliases[country]) else character()
  names <- unique(c(country, alternate))
  x <- links[vapply(Label, function(z) any(vapply(names, function(n) grepl(paste0("^", gsub("([][{}()+*^$|\\\\?.])", "\\\\\\\\\\1", n), "(?: |$)"), z, ignore.case=TRUE, perl=TRUE), logical(1))), logical(1))]
  x <- x[grepl(as.character(year), Label, fixed=TRUE)]
  if (!nrow(x)) return(data.table())
  x[order(nchar(Href), Href)][1]
}
message("Discovering exact RSSSF links for ", nrow(gaps), " missing top-flight seasons...")
manifest <- rbindlist(lapply(seq_len(nrow(gaps)), function(i) {
  if (i %% 50L == 0L || i == nrow(gaps)) message("  Discovered ", i, "/", nrow(gaps), " top-flight candidates")
  x <- gaps[i]; z <- find_link(x$Country, x$StartYear)
  if (!nrow(z)) return(data.table(Country=x$Country, StartYear=x$StartYear, Tier=1L, Scope="top_flight", Href=NA_character_, Status="not_found_in_archive"))
  data.table(Country=x$Country, StartYear=x$StartYear, Tier=1L, Scope="top_flight", Href=z$Href, Status="discovered")
}), fill=TRUE)

# Wales tier 2 is intentionally a fixed known series, rather than inferred from
# country gaps: the same national page contains Cymru North and Cymru South.
wales <- data.table(Country="Wales", StartYear=min_year:max_year, Tier=2L, Scope="wales_tier2", Href=sprintf("tablesw/wal%d.html", min_year:max_year), Status="discovered")
manifest <- rbindlist(list(manifest, wales), fill=TRUE)
manifest[, Url := fifelse(grepl("^https?://", Href), Href, paste0("https://www.rsssf.org/", sub("^/", "", Href)))]
manifest[, RelativeFile := sub("^https?://[^/]+/", "", Url)]
manifest[, LocalFile := file.path(pages, RelativeFile)]
message("Prepared ", nrow(manifest), " source-page candidates (including Wales tier 2).")
for (i in seq_len(nrow(manifest))) {
  p <- manifest$LocalFile[i]; dir.create(dirname(p), recursive=TRUE, showWarnings=FALSE)
  if (file.exists(p)) { manifest$Status[i] <- "already_cached"; next }
  if (!apply_download || manifest$Status[i] != "discovered") next
  message(sprintf("[%d/%d] Downloading %s", i, nrow(manifest), manifest$Url[i]))
  ok <- tryCatch({ download.file(manifest$Url[i], p, mode="wb", quiet=TRUE); file.exists(p) && file.info(p)$size > 500 }, error=function(e) FALSE)
  manifest$Status[i] <- if (ok) "downloaded" else "download_failed"
  if (file.exists(p) && !ok) unlink(p)
  Sys.sleep(delay)
}
setorder(manifest, Tier, Country, StartYear)
fwrite(manifest, file.path(out, "page_manifest.csv"), na="")
print(manifest[, .N, by=.(Tier, Scope, Status)][order(Tier, Scope, Status)])
message("Manifest: ", file.path(out, "page_manifest.csv"))
message(if (apply_download) "Downloads complete. Run stage 2." else "Preview only. Set DOWNLOAD_RSSSF_GAP_RECOVERY='1' and rerun stage 1 to download.")
