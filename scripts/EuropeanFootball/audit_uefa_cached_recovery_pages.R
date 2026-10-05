# Review the cached pages flagged by online discovery. No web requests/imports.
run_uefa_cached_recovery <- function(manifest_path=NULL,
                                   output_folder="UEFA/cached_page_recovery",
                                   completion_beep=TRUE) {
  on.exit({
    if (isTRUE(completion_beep) && interactive() && requireNamespace("beepr", quietly=TRUE))
      try(beepr::beep(), silent=TRUE)
  }, add=TRUE)
  suppressPackageStartupMessages(library(data.table))
  root <- normalizePath(Sys.getenv("J_RATINGS_REPO", getwd()), winslash="/", mustWork=TRUE)
  base <- file.path(root,"EuropeanFootball/pipeline_data")
  out <- file.path(base,"Manual_Sources",output_folder)
  dir.create(out,recursive=TRUE,showWarnings=FALSE)
  if(is.null(manifest_path)) manifest_path <- file.path(base,"Manual_Sources/UEFA/online_page_discovery/recovery_opportunities.csv")
  manifest <- fread(manifest_path)
  manifest <- manifest[LocalCached == TRUE]
  if (!nrow(manifest)) stop("No cached recovery pages in the discovery report.")
  # Use isolated staging directories so no other countries/seasons are parsed.
  manifest[, Directory := paste0("uefa_cached_recovery/",gsub("[^A-Za-z0-9]","_",Country))]
  manifest[, OriginalFile := vapply(strsplit(LocalFiles," | ",fixed=TRUE), `[`, character(1),1L)]
  manifest[, Filename := basename(OriginalFile)]
  manifest[, StagedFile := file.path(base,"Source/rsssf/all/review/pages",Directory,Filename)]
  for (i in seq_len(nrow(manifest))) {
    if (!file.exists(manifest$OriginalFile[i])) stop("Missing cached file: ",manifest$OriginalFile[i])
    dir.create(dirname(manifest$StagedFile[i]),recursive=TRUE,showWarnings=FALSE)
    if (!file.copy(manifest$OriginalFile[i],manifest$StagedFile[i],overwrite=TRUE)) stop("Staging failed.")
  }
  fwrite(manifest,file.path(out,"source_manifest.csv"))
  cfg <- manifest[, .(Directory=Directory[1],
    FilePattern=paste0("^(",paste(gsub("[.]","[.]",unique(Filename)),collapse="|"),")$"),
    Confederation="UEFA",Competition=paste0(Country[1]," - Top flight"),CompetitionType="league"),by=Country]
  env_names <- c("RSSSF_OFC_FUNCTIONS_ONLY","UEFA_CACHED_RECOVERY_RESUME",
                 "UEFA_CACHED_RECOVERY_MIN_YEAR","UEFA_CACHED_RECOVERY_MAX_YEAR")
  old <- Sys.getenv(env_names,unset=NA_character_)
  on.exit(for (i in seq_along(env_names)) {
    if (is.na(old[i])) Sys.unsetenv(env_names[i]) else do.call(Sys.setenv,setNames(list(old[i]),env_names[i]))
  },add=TRUE)
  Sys.setenv(RSSSF_OFC_FUNCTIONS_ONLY="1",UEFA_CACHED_RECOVERY_RESUME="0",
             UEFA_CACHED_RECOVERY_MIN_YEAR="2009",UEFA_CACHED_RECOVERY_MAX_YEAR="2025")
  engine <- new.env(parent=globalenv())
  sys.source(file.path(root,"scripts/EuropeanFootball/rsssf_ofc_audit.R"),envir=engine)
  cat("Parsing ",nrow(manifest)," cached UEFA pages. No downloads.\n",sep="")
  parsed <- engine$run_rsssf_ofc_audit(config_override=cfg,
    audit_name="UEFA cached recovery",output_folder=paste0(output_folder,"/parser"),
    env_prefix="UEFA_CACHED_RECOVERY",completion_beep=FALSE)
  cat("Comparing parsed games with the production master...\n")
  master <- fread(file.path(base,"Matches_Clean_Combined/european_football_all_matches.csv"),showProgress=FALSE)
  master <- master[Country %in% manifest$Country & CompetitionType=="league" & as.integer(Tier)==1L]
  master[, Date:=as.IDate(Date)]
  aliases <- fread(file.path(base,"Reference/team_aliases.csv"))
  aliases <- unique(aliases[,.(Country,SourceName,CanonicalName)])
  # Only use existing unambiguous aliases. Never guess a new identity here.
  aliases <- aliases[, if(uniqueN(CanonicalName)==1L) .(CanonicalName=CanonicalName[1]) else NULL,
                     by=.(Country,SourceName)]
  amap <- setNames(aliases$CanonicalName,paste(aliases$Country,aliases$SourceName,sep="\r"))
  canon <- function(country,x) {
    hit <- unname(amap[paste(country,x,sep="\r")]); x[!is.na(hit)] <- hit[!is.na(hit)]; x
  }
  norm <- function(x) gsub("[^a-z0-9]","",tolower(stringi::stri_trans_general(x,"Latin-ASCII")))
  fixture_key <- function(dt) paste(dt$Country,dt$Date,norm(canon(dt$Country,dt$Home)),
                                    norm(canon(dt$Country,dt$Away)),dt$Score,sep="\r")
  master[, FixtureKey:=fixture_key(master)]
  g <- copy(parsed$games)
  if (nrow(g)) {
    g[, Date:=as.IDate(Date)]
    g <- g[!is.na(Date) & !(tolower(as.character(Annotated)) %chin% c("true","t","1"))]
    g[, FixtureKey:=fixture_key(g)]
    g[, MatchedInMaster:=FixtureKey %chin% master$FixtureKey]
  }
  comparison <- copy(parsed$audit)
  comparison[, `:=`(MasterMatchesSameSeason=0L,MasterMatchesInDateWindow=0L,
                     ParsedMatchesFoundInMaster=0L,ParsedMatchesNotMatched=0L)]
  for (i in seq_len(nrow(comparison))) {
    country <- comparison$Country[i]; season <- comparison$Season[i]
    page <- comparison$SourceFile[i]
    pg <- if(nrow(g)) g[SourceFile==page] else g
    same <- master[Country==country & Season==season]
    window <- if(nrow(pg)) master[Country==country & Date>=min(pg$Date) & Date<=max(pg$Date)] else same[0]
    comparison[i, `:=`(MasterMatchesSameSeason=nrow(same),MasterMatchesInDateWindow=nrow(window),
      ParsedMatchesFoundInMaster=if(nrow(pg)) sum(pg$MatchedInMaster) else 0L,
      ParsedMatchesNotMatched=if(nrow(pg)) sum(!pg$MatchedInMaster) else 0L)]
  }
  comparison[, ReviewStatus:=fcase(
    nzchar(Error),"PARSER_ERROR",
    ParsedMatchesFoundInMaster>0L & MasterMatchesSameSeason==0L,"CHECK_SEASON_LABEL",
    ParsedMatchesNotMatched>0L,"REVIEW_UNMATCHED_FIXTURES_AND_STRUCTURE",
    RSSSFDatedResults==0L,"NO_DATED_RESULTS_EXTRACTED",
    default="ALREADY_REPRESENTED")]
  fwrite(comparison,file.path(out,"season_comparison.csv"),na="")
  if(nrow(g)) fwrite(g[MatchedInMaster==FALSE],file.path(out,"unmatched_parsed_games.csv"),na="")
  print(comparison[,.(Country,Season,RSSSFPlayedResults,RSSSFDatedResults,
    MasterMatchesSameSeason,MasterMatchesInDateWindow,ParsedMatchesFoundInMaster,
    ParsedMatchesNotMatched,ReviewStatus)])
  cat("\nUnmatched games require review: names, playoff inclusion and dates can cause false differences.\n")
  cat("The dated percentage measures extracted rows, not completeness of the whole season.\n")
  cat("Review: ",file.path(out,"season_comparison.csv"),"\nProduction master unchanged.\n",sep="")
}
if(!identical(Sys.getenv("UEFA_RECOVERY_FUNCTIONS_ONLY"),"1")) run_uefa_cached_recovery()
