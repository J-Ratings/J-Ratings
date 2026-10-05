# Monthly source refresh and review only. Never imports matches or rewrites Elo.
local({
  suppressPackageStartupMessages(library(data.table))
  root <- normalizePath(Sys.getenv('J_RATINGS_REPO', getwd()), winslash='/', mustWork=TRUE)
  base <- file.path(root,'EuropeanFootball/pipeline_data')
  out <- file.path(base,'Manual_Sources/RSSSF_Monthly_Refresh')
  dir.create(out,recursive=TRUE,showWarnings=FALSE)
  if (interactive()) on.exit(try(beepr::beep(),silent=TRUE),add=TRUE)
  # These top flights already have functioning current OpenFootball files.
  # Override this comma-separated list when an alternative stops working.
  excluded <- strsplit(Sys.getenv('RSSSF_REFRESH_EXCLUDE_COUNTRIES',
    'England,Spain,Italy,Germany,France,Portugal,Netherlands,Austria,Switzerland,Scotland,Greece,Czechia'),',',fixed=TRUE)[[1]]
  seeds <- unique(fread(file.path(base,'Reference/non_uefa_country_seeds.csv'))[Tier==1,.(Country,Confederation)])
  today <- Sys.Date(); year <- as.integer(format(today,'%Y'))
  start_year <- max(2025L,year-2L)
  cutoff <- as.Date('2024-12-31')
  fetch <- function(url,dest,optional=FALSE) {
    Sys.sleep(1.5)
    tmp <- paste0(dest,'.tmp')
    ok <- tryCatch({download.file(url,tmp,quiet=TRUE,mode='wb'); TRUE},error=function(e) {
      msg <- conditionMessage(e)
      if (grepl('429|403',msg)) stop('Source access restriction; stopping: ',msg)
      if (!optional || !grepl('404',msg)) stop(msg)
      FALSE
    })
    if (!ok) {unlink(tmp);return(FALSE)}
    if (!file.copy(tmp,dest,overwrite=TRUE)) stop('Cannot save ',dest)
    unlink(tmp); TRUE
  }
  index_file <- file.path(out,'curdom.html')
  message('[1/3] Refreshing RSSSF current index...')
  fetch('https://www.rsssf.org/curdom.html',index_file)
  doc <- xml2::read_html(index_file)
  a <- xml2::xml_find_all(doc,'//a[@href]')
  links <- data.table(Country=trimws(xml2::xml_text(a)),Href=xml2::xml_attr(a,'href'))
  renames <- c(Hongkong='Hong Kong',Macao='Macau',Surinam='Suriname','Congo-Kinshasa'='DR Congo',
    'Congo-Brazzaville'='Congo','East Timor'='Timor-Leste','US Virgin Islands'='United States Virgin Islands')
  links[Country %chin% names(renames), Country:=unname(renames[Country])]
  links <- merge(links,seeds,by='Country')[!Country %chin% excluded & grepl('[.]html',Href)]
  links <- unique(links,by='Country')
  folder <- file.path(base,'Source/rsssf/all/review/pages/monthly_refresh')
  dir.create(folder,recursive=TRUE,showWarnings=FALSE)
  configs <- list(); log <- list()
  message('[2/3] Refreshing recent pages; 1.5 seconds between requests...')
  for (i in seq_len(nrow(links))) {
    r <- links[i]; url <- sub('#.*$','',xml2::url_absolute(r$Href,'https://www.rsssf.org/curdom.html'))
    if (!grepl('^https://www[.]rsssf[.]org/',url)) next
    name <- basename(url)
    if (!grepl('20[0-9]{2}[a-z]?[.]html$',name)) next
    prefix <- sub('20[0-9]{2}[a-z]?[.]html$','',name)
    urls <- unique(c(url,vapply(start_year:year,function(y)
      sub('20[0-9]{2}[a-z]?[.]html$',paste0(y,'.html'),url),character(1))))
    message(sprintf('  Country %d/%d: %s',i,nrow(links),r$Country))
    for (u in urls) {
      dest <- file.path(folder,basename(u))
      prior <- if(file.exists(dest)) unname(tools::md5sum(dest)) else ''
      ok <- fetch(u,dest,optional=TRUE)
      log[[length(log)+1L]] <- data.table(Country=r$Country,URL=u,CheckedAt=as.character(Sys.time()),
        Status=if(!ok)'NOT_FOUND' else if(identical(prior,unname(tools::md5sum(dest))))'UNCHANGED' else 'NEW_OR_CHANGED')
    }
    configs[[length(configs)+1L]] <- data.table(Country=r$Country,Confederation=r$Confederation,
      Directory='monthly_refresh',FilePattern=paste0('^',prefix,'20[0-9]{2}[a-z]?[.]html$'),
      Competition=paste(r$Country,'Top Flight'),CompetitionType='league')
    fwrite(rbindlist(log),file.path(out,'download_review.csv'))
  }
  message('[3/3] Parsing refreshed pages into review outputs...')
  Sys.setenv(RSSSF_OFC_FUNCTIONS_ONLY='1',MONTHLY_RSSSF_RESUME='0')
  Sys.setenv(MONTHLY_RSSSF_MIN_YEAR=as.character(start_year),MONTHLY_RSSSF_MAX_YEAR=as.character(year+1L))
  on.exit(Sys.unsetenv(c('RSSSF_OFC_FUNCTIONS_ONLY','MONTHLY_RSSSF_RESUME','MONTHLY_RSSSF_MIN_YEAR','MONTHLY_RSSSF_MAX_YEAR')),add=TRUE)
  source(file.path(root,'scripts/EuropeanFootball/rsssf_ofc_audit.R'),local=TRUE)
  result <- run_rsssf_ofc_audit(config_override=rbindlist(configs),audit_name='monthly RSSSF refresh',
    output_folder='RSSSF_Monthly_Refresh/parser',env_prefix='MONTHLY_RSSSF',completion_beep=FALSE)
  if(nrow(result$games)) fwrite(result$games[!is.na(Date) & Date>cutoff],file.path(out,'post_checkpoint_review_matches.csv'))
  message('Finished. Master and checkpoint unchanged. Review new/changed pages and candidate matches in ',out)
})
