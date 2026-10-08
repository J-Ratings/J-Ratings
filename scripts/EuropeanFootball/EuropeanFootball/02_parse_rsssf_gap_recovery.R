# Stage 2: parse the pages downloaded by stage 1 and write a review-only audit.
# No production master changes.
suppressPackageStartupMessages({ library(data.table); library(xml2) })
root <- normalizePath(Sys.getenv("J_RATINGS_REPO", getwd()), winslash="/", mustWork=TRUE)
base <- file.path(root,"EuropeanFootball/pipeline_data")
recovery <- file.path(base,"Manual_Sources/RSSSF_Gap_Recovery")
manifest <- fread(file.path(recovery,"page_manifest.csv"))
manifest <- manifest[Status %chin% c("downloaded","already_cached") & file.exists(LocalFile)]
if (!nrow(manifest)) stop("No downloaded pages found. Run stage 1 first.")

# This recovery pass is deliberately Wales-only.  Other downloaded pages are
# retained in the local cache for a later country-by-country audit.
manifest <- manifest[Country == "Wales"]
if (!nrow(manifest)) stop("The stage-1 manifest contains no downloaded Wales pages.")

# Put review copies in the existing parser cache.  They remain review inputs;
# stage 3 is the only script allowed to alter the master.
review_root <- file.path(base,"Source/rsssf/all/review/pages")
for (i in seq_len(nrow(manifest))) {
  dest <- file.path(review_root, manifest$RelativeFile[i]); dir.create(dirname(dest), recursive=TRUE, showWarnings=FALSE)
  if (!file.exists(dest)) file.copy(manifest$LocalFile[i], dest)
}

seed <- fread(file.path(base,"Reference/non_uefa_country_seeds.csv"))[Tier==1, .(Country, Confederation)]
seed <- unique(seed)
conf <- setNames(seed$Confederation, seed$Country); conf[is.na(conf)] <- "UEFA"
t1 <- manifest[Tier==1]
t1[, `:=`(Directory=dirname(RelativeFile), Filename=basename(RelativeFile),
  Confederation=unname(conf[Country]), Competition=paste(Country,"Top Flight"), CompetitionType="league")]
t1[is.na(Confederation), Confederation:="Unmapped"]
t1[, FilePattern := paste0("^", gsub("[.]", "[.]", Filename), "$")]
configs <- unique(t1[,.(Country,Confederation,Directory,FilePattern,Competition,CompetitionType)])
if (nrow(configs)) {
  message("Parsing ", nrow(configs), " recovered Wales top-flight pages...")
  Sys.setenv(RSSSF_OFC_FUNCTIONS_ONLY="1")
  source(file.path(root,"scripts/EuropeanFootball/rsssf_ofc_audit.R"), encoding="UTF-8")
  Sys.unsetenv("RSSSF_OFC_FUNCTIONS_ONLY")
  Sys.setenv(GAP_RECOVERY_MIN_YEAR="2010", GAP_RECOVERY_MAX_YEAR="2025", GAP_RECOVERY_RESUME="0")
  t1_result <- run_rsssf_ofc_audit(config_override=configs, audit_name="gap recovery top flight", output_folder="RSSSF_Gap_Recovery/tier1", env_prefix="GAP_RECOVERY")
} else message("No missing Wales top-flight page was downloaded in this 2010–25 recovery window.")

# Wales tier 2 is inspected separately.  It is deliberately not passed through
# the top-flight isolator.  The section label is retained for manual review.
message("Parsing ", nrow(manifest[Tier==2]), " Wales tier-2 pages...")
Sys.setenv(FOUR_LEAGUES_FUNCTIONS_ONLY="1")
engine <- new.env(parent=globalenv())
source(file.path(root,"scripts/EuropeanFootball/wikipedia_rsssf_four_leagues.R"), local=engine, encoding="UTF-8")
Sys.unsetenv("FOUR_LEAGUES_FUNCTIONS_ONLY")
wales <- manifest[Tier==2]
dir.create(file.path(recovery,"wales_tier2"),recursive=TRUE,showWarnings=FALSE)
wales_out <- rbindlist(lapply(seq_len(nrow(wales)), function(i) {
  x <- wales[i]
  # The common parser does not preserve the named subheading on these older
  # pages.  Give it only the HTML block for the second level instead.
  raw <- paste(readLines(x$LocalFile, warn=FALSE, encoding="UTF-8"), collapse="\n")
  # RSSSF's walYYYY filename uses the season's ending year: wal2011 is
  # 2010/11.  Prefer the year printed in the page title and use that filename
  # convention only as a fallback.
  title_match <- regexec("(?is)<title[^>]*>[^<]*Wales[^0-9]*([12][0-9]{3})/[0-9]{2,4}", raw, perl=TRUE)
  title_parts <- regmatches(raw, title_match)[[1L]]
  season_start <- if (length(title_parts) >= 2L) as.integer(title_parts[2L]) else as.integer(x$StartYear) - 1L
  message(sprintf("  Wales tier 2: %d/%d (%d/%02d)",i,nrow(wales),season_start,(season_start+1L) %% 100L))
  start <- regexpr("(?is)<h4[^>]*>\\s*<a[^>]*>\\s*Second Level", raw, perl=TRUE)
  if (start[1] < 0L) return(data.table(StartYear=season_start, SourceFile=x$LocalFile, Played=0L,Dated=0L,DatedPercent=NA_real_,Quality="review",Error="second-level block not found"))
  tail <- substr(raw, start[1], nchar(raw))
  stop_at <- regexpr("(?is)<h4[^>]*>\\s*<a[^>]*>\\s*(Third Level|Fourth Level)", tail, perl=TRUE)
  block <- if (stop_at[1] > 0L) substr(tail, 1L, stop_at[1]-1L) else tail
  # The shared top-flight parser quite correctly treats a heading named
  # "Second Level" as a stop boundary.  This script has already isolated that
  # exact block. Split its named north/south sub-sections and give each
  # in-memory copy a neutral heading. This also preserves the actual league
  # identity instead of collapsing both parallel competitions into one.
  subsection_pattern <- "(?is)<b[^>]*>\\s*<a[^>]*name=[\"']wal2[^\"']+[\"'][^>]*>.*?</a>\\s*</b>"
  loc <- gregexpr(subsection_pattern, block, perl=TRUE)[[1L]]
  parsed <- list()
  if (loc[1L] > 0L) {
    lens <- attr(loc,"match.length")
    labels <- regmatches(block, list(loc))[[1L]]
    labels <- vapply(labels, function(z) xml_text(read_html(paste0("<div>",z,"</div>"))), character(1L))
    for (k in seq_along(loc)) {
      content_start <- loc[k] + lens[k]
      content_end <- if (k < length(loc)) loc[k+1L] - 1L else nchar(block)
      content <- substr(block, content_start, content_end)
      content <- gsub("(?is)</?pre[^>]*>", "", content, perl=TRUE)
      doc_text <- paste0("<html><body><h4>Welsh Tier Two League</h4><pre>",content,"</pre></body></html>")
      part <- tryCatch(as.data.table(engine$rsssf_games(read_html(doc_text), season_start, season_start+1L)), error=function(e) data.table(Error=conditionMessage(e)))
      if ("Error" %in% names(part)) { parsed <- list(part); break }
      if (nrow(part)) part[, Competition := labels[k]]
      parsed[[length(parsed)+1L]] <- part
    }
    g <- rbindlist(parsed, fill=TRUE)
  } else {
    block <- sub("(?is)^<h4[^>]*>.*?</h4>", "<h4>Welsh Tier Two League</h4>", block, perl=TRUE)
    g <- tryCatch(as.data.table(engine$rsssf_games(read_html(paste0("<html><body>",block,"</body></html>")), season_start, season_start+1L)), error=function(e) data.table(Error=conditionMessage(e)))
    if (nrow(g)) g[, Competition := "Wales Tier Two"]
  }
  if ("Error" %in% names(g)) return(data.table(StartYear=season_start, SourceFile=x$LocalFile, Played=0L,Dated=0L,DatedPercent=NA_real_,Quality="parse_error",Error=g$Error[1]))
  annotated <- if ("Annotated" %in% names(g)) {
    tolower(trimws(as.character(g$Annotated))) %chin% c("true", "t", "1", "yes")
  } else rep(FALSE, nrow(g))
  played <- sum(!annotated)
  dated <- sum(!is.na(g$Date) & !annotated)
  pct <- if(played) round(100*dated/played,2) else NA_real_
  fwrite(g, file.path(recovery,"wales_tier2",sprintf("%d_games.csv",season_start)), na="")
  data.table(StartYear=season_start,SourceFile=x$LocalFile,Played=played,Dated=dated,DatedPercent=pct,Quality=if(played>=100 && pct>=95) "eligible" else "review",Error="")
}), fill=TRUE)
fwrite(wales_out,file.path(recovery,"wales_tier2","season_audit.csv"),na="")
message("Review top flight: ",file.path(recovery,"tier1","season_audit.csv"))
message("Review Wales tier 2: ",file.path(recovery,"wales_tier2","season_audit.csv"))
message("Production master unchanged.")
