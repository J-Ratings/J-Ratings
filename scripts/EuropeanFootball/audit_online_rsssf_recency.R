# Read-only online recency check. No master, aliases or ratings are changed.
local({
  root <- normalizePath(Sys.getenv('J_RATINGS_REPO', getwd()), winslash='/', mustWork=TRUE)
  library(data.table)
  out <- file.path(root, 'EuropeanFootball/pipeline_data/Manual_Sources/Season_2025_26/online_rsssf_recency')
  dir.create(out, recursive=TRUE, showWarnings=FALSE)
  on.exit(if (requireNamespace('beepr', quietly=TRUE)) try(beepr::beep(), silent=TRUE), add=TRUE)
  ranked <- fread(file.path(root, 'EuropeanFootball/pipeline_data/Manual_Sources/Season_2025_26/source_recency/elo_ranked_league_recency.csv'))[1:30]
  ranked[, Country := sub(' - .*$', '', League)]
  stage <- file.path(root, 'EuropeanFootball/pipeline_data/Source/rsssf/all/review/pages/online_recency')
  dir.create(stage, recursive=TRUE, showWarnings=FALSE)
  fetched <- character()
  fetch <- function(url) {
    if (!grepl('^https://www[.]rsssf[.]org/', url)) stop('Unexpected source host: ', url)
    path <- file.path(out, paste0(gsub('[^A-Za-z0-9_.-]', '_', sub('https://www.rsssf.org/', '', url)), '.snapshot'))
    if (!file.exists(path)) {
      Sys.sleep(1.5)
      message('  Downloading ', url)
      tmp <- paste0(path, '.tmp')
      status <- tryCatch(download.file(url, tmp, quiet=TRUE, mode='wb'), error=function(e) stop('Request failed; stopping and retaining successful snapshots: ', conditionMessage(e)))
      if (status != 0L) stop('Download failed: ', url)
      if (!file.rename(tmp, path)) stop('Cannot save snapshot')
    }
    xml2::read_html(path)
  }
  message('[1/3] Reading current online RSSSF index...')
  index <- fetch('https://www.rsssf.org/curdom.html')
  a <- xml2::xml_find_all(index, '//a[@href]')
  links <- data.table(Country=trimws(xml2::xml_text(a)), Href=xml2::xml_attr(a, 'href'))
  configs <- list(); evidence <- list()
  countries <- unique(ranked$Country)
  for (i in seq_along(countries)) {
    country <- countries[i]
    message(sprintf('[%d/%d] Checking %s', i, length(countries), country))
    hit <- links[Country == country & grepl('[.]html', Href)]
    if (!nrow(hit)) { evidence[[i]] <- data.table(Country=country, URL='', Status='INDEX_LINK_REVIEW'); next }
    url <- xml2::url_absolute(hit$Href[1], 'https://www.rsssf.org/curdom.html')
    doc <- fetch(url)
    # Follow only published links to recent editions of this same country file.
    filename <- basename(sub('#.*$', '', url))
    prefix <- sub('(19|20)?[0-9]{2}[a-z]?[.]html$', '', filename)
    aa <- xml2::xml_find_all(doc, '//a[@href]')
    other <- xml2::url_absolute(xml2::xml_attr(aa, 'href'), url)
    urls <- unique(sub('#.*$', '', c(url, other)))
    urls <- urls[startsWith(urls, 'https://www.rsssf.org/') & startsWith(basename(urls), prefix) & grepl('202[4-7][a-z]?[.]html$', urls)]
    if (!length(urls)) urls <- sub('#.*$', '', url)
    for (u in urls) {
      page <- if (identical(u, sub('#.*$', '', url))) doc else fetch(u)
      target <- file.path(stage, basename(u))
      writeLines(as.character(page), target, useBytes=TRUE)
      evidence[[length(evidence)+1L]] <- data.table(Country=country, URL=u, Status='DOWNLOADED_FOR_PARSER_CHECK')
    }
    configs[[length(configs)+1L]] <- data.table(Country=country, Confederation='recency', Directory='online_recency', FilePattern=paste0('^', prefix, '202[4-7][a-z]?[.]html$'), Competition=paste(country, 'Top Flight'), CompetitionType='league')
  }
  fwrite(rbindlist(evidence, fill=TRUE), file.path(out, 'online_pages.csv'))
  message('[2/3] Parsing recent top-flight pages...')
  Sys.setenv(RSSSF_OFC_FUNCTIONS_ONLY='1', ONLINE_RECENCY_MIN_YEAR='2024', ONLINE_RECENCY_MAX_YEAR='2027', ONLINE_RECENCY_RESUME='0')
  on.exit(Sys.unsetenv(c('RSSSF_OFC_FUNCTIONS_ONLY','ONLINE_RECENCY_MIN_YEAR','ONLINE_RECENCY_MAX_YEAR','ONLINE_RECENCY_RESUME')), add=TRUE)
  source(file.path(root, 'scripts/EuropeanFootball/rsssf_ofc_audit.R'), local=TRUE)
  parsed <- run_rsssf_ofc_audit(config_override=rbindlist(configs), audit_name='online recency', output_folder='Season_2025_26/online_rsssf_recency/parser', env_prefix='ONLINE_RECENCY', completion_beep=FALSE)
  g <- as.data.table(parsed$games)
  summary <- if (nrow(g)) g[!is.na(Date), .(ExtractedDatedResults=.N, LatestExtractedDate=as.character(max(as.Date(Date)))), by=Country] else data.table(Country=character(), ExtractedDatedResults=integer(), LatestExtractedDate=character())
  result <- merge(ranked, summary, by='Country', all.x=TRUE, sort=FALSE)
  result[, Finding := fifelse(is.na(LatestExtractedDate), 'NO_DATED_RESULTS_EXTRACTED: inspect page, not proof of absence', 'TOP_FLIGHT_PARSER_RESULT: scope/date verification required')]
  # These competitions need their own division extraction; country top-flight dates cannot answer them.
  result[grepl('Championship|Segunda|2[.] Bundesliga|Ligue 2|Serie B', League), `:=`(LatestExtractedDate=NA_character_, ExtractedDatedResults=NA_integer_, Finding='LOWER_TIER_SECTION_REVIEW: country top-flight result does not apply')]
  setorder(result, EloRank)
  fwrite(result, file.path(out, 'top_30_online_recency.csv'))
  message('[3/3] Online recency extraction complete:')
  print(result[, .(EloRank, League, LatestExtractedDate, ExtractedDatedResults, Finding)])
  message('Report: ', out, '\nProduction unchanged. Extraction dates are evidence, not certification of completeness.')
})
