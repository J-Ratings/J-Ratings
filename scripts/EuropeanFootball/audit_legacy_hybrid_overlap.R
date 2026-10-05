# Read-only targeted overlap audit; neither master nor aliases are rewritten.
local({
  suppressPackageStartupMessages({library(data.table);library(stringi)})
  started <- Sys.time()
  base <- 'EuropeanFootball/pipeline_data'
  out <- file.path(base,'Manual_Sources/Team_Identity_Audit/legacy_hybrid_overlap')
  dir.create(out,recursive=TRUE,showWarnings=FALSE)
  cat('[1/4] Reading master and existing aliases...\n')
  path <- file.path(base,'Matches_Clean_Combined/european_football_all_matches.csv')
  fingerprint <- unname(tools::md5sum(path))
  m <- fread(path,showProgress=FALSE,encoding='UTF-8')
  m[,RowID:=seq_len(.N)]
  m[,Hybrid:=grepl('wikipedia.*rsssf',Source,ignore.case=TRUE)]
  a <- fread(file.path(base,'Reference/team_aliases.csv'),encoding='UTF-8')
  source('scripts/EuropeanFootball/club_identity_resolution.R',local=TRUE)
  amap <- setNames(a$CanonicalName,paste(a$Country,a$SourceName,sep='\r'))
  norm <- function(x) gsub('[^a-z0-9]','',tolower(stri_trans_general(x,'Latin-ASCII')))
  # Restrict to the associations represented by hybrids, but not their seasons:
  # erroneous season labels must not hide duplicates on the same date.
  h <- m[Hybrid==TRUE]
  cat('Hybrid-labelled rows:',nrow(h),'in',uniqueN(h$Country),'countries\n')
  q <- m[Country %chin% h$Country]
  q[,`:=`(D=as.IDate(Date),H=norm(resolve_alias_chain(Home,Country,amap,setNames(character(),character()))),
           A=norm(resolve_alias_chain(Away,Country,amap,setNames(character(),character()))))]
  q <- q[!is.na(D) & nzchar(H) & nzchar(A) & grepl('^[0-9]+-[0-9]+$',Score)]
  x <- q[Hybrid==TRUE,.(HybridRow=RowID,Country,Tier,H,A,HybridDate=D,
                        HybridScore=Score,HybridCompetition=Competition,HybridSeason=Season)]
  y <- q[,.(OtherRow=RowID,Country,Tier,H,A,OtherDate=D,OtherScore=Score,
             OtherCompetition=Competition,OtherSeason=Season,OtherHybrid=Hybrid,OtherSource=Source)]
  cat('[2/4] Comparing alias-resolved pairs, including nearby dates...\n')
  p <- merge(x,y,by=c('Country','Tier','H','A'),allow.cartesian=TRUE)
  p <- p[HybridRow!=OtherRow & (!OtherHybrid | HybridRow<OtherRow)]
  p[,DayDifference:=as.integer(OtherDate-HybridDate)]
  p <- p[abs(DayDifference)<=7L]
  p[,Decision:=fcase(DayDifference==0L & HybridScore==OtherScore,'SAME_DATE_PAIR_SCORE',
                     DayDifference==0L,'SAME_DATE_PAIR_DIFFERENT_SCORE',
                     HybridScore==OtherScore,'NEARBY_DATE_PAIR_SCORE',
                     default='NEARBY_PAIR_DIFFERENT_SCORE')]
  p[,SameCompetition:=HybridCompetition==OtherCompetition]
  fwrite(p,file.path(out,'pair_overlap_candidates.csv'))
  cat('[3/4] Checking same-date/score evidence with one known opponent...\n')
  # These are naming-review clues only: matching score and one club is not proof.
  xo <- melt(x,id.vars=c('HybridRow','Country','Tier','HybridDate','HybridScore'),
             measure.vars=c('H','A'),variable.name='Side',value.name='KnownClub')
  yo <- melt(y,id.vars=c('OtherRow','Country','Tier','OtherDate','OtherScore','OtherHybrid'),
             measure.vars=c('H','A'),variable.name='Side',value.name='KnownClub')
  setnames(xo,c('HybridDate','HybridScore'),c('D','Score'))
  setnames(yo,c('OtherDate','OtherScore'),c('D','Score'))
  names_review <- merge(xo,yo,by=c('Country','Tier','D','Score','Side','KnownClub'),allow.cartesian=TRUE)
  names_review <- names_review[HybridRow!=OtherRow & (!OtherHybrid | HybridRow<OtherRow)]
  exact_ids <- p[DayDifference==0L,paste(HybridRow,OtherRow)]
  names_review <- names_review[!paste(HybridRow,OtherRow) %chin% exact_ids]
  fwrite(names_review,file.path(out,'one_opponent_name_review.csv'))
  ids <- unique(c(p$HybridRow,p$OtherRow,names_review$HybridRow,names_review$OtherRow))
  fwrite(m[RowID %in% ids],file.path(out,'candidate_master_rows.csv'))
  summary <- h[,.(HybridRows=.N),by=.(Country,Source)]
  fwrite(summary,file.path(out,'hybrid_inventory.csv'))
  cat('[4/4] Overlap summary:\n')
  print(p[,.N,by=.(Decision,OtherHybrid,SameCompetition)])
  cat('Distinct hybrid rows with exact date/pair/score overlap:',
      uniqueN(c(p[Decision=='SAME_DATE_PAIR_SCORE',HybridRow],
                p[Decision=='SAME_DATE_PAIR_SCORE' & OtherHybrid==TRUE,OtherRow])),'\n')
  cat('One-opponent naming review pairs:',nrow(names_review),'\n')
  stopifnot(identical(fingerprint,unname(tools::md5sum(path))))
  writeLines(c(paste('Master MD5:',fingerprint),
    'Names resolved using existing aliases, then accent/punctuation normalisation. No new fuzzy aliases.',
    'Comparisons are country/tier scoped, ordered home/away; no season constraint.',
    'Nearby-date window: seven days. One-opponent evidence is review only.',
    'Different competitions/stages can share opponents: overlap flags are not deletion approvals.',
    'Rows without valid dates/scores cannot be checked by fixture keys.',
    'Master and aliases unchanged.'),file.path(out,'READ_ME.txt'))
  cat('Report:',normalizePath(out,winslash='/'),'\nElapsed:',
      round(as.numeric(difftime(Sys.time(),started,units='secs'))),'seconds\n')
})
