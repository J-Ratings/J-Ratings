# Small offline diagnostic, not the full audit; no production or audit overwrite.
tictoc::tic("Cached parser regression checks")
Sys.setenv(FOUR_LEAGUES_FUNCTIONS_ONLY="1")
source("scripts/EuropeanFootball/wikipedia_rsssf_four_leagues.R")
stopifnot(!name_related("Arka","Siarka"))
stopifnot(rsssf_date("Nov 8; postponed from Sep 9,10",2022L,2023L)==as.Date("2022-11-08"),
  is.na(rsssf_date("Sep 9; postponed to Nov 8",2022L,2023L)))
typo_doc <- read_html('<html><pre>Nov 25]
Alpha 1-0 Beta
[Nov 26]
Gamma 2-0 Delta [replay]
[Nov 27]
Epsilon 1-0 Zeta [replay; awarded]
</pre></html>')
typo_games <- rsssf_games(typo_doc,2022L,2023L)
stopifnot(identical(typo_games$Date,as.Date(c("2022-11-25","2022-11-26",NA))))
stopifnot(rsssf_other_competition("Latvijas Kauss 2025"),
  rsssf_other_competition("Evald Tipneri Karikas 2024/25"),
  rsssf_other_competition("Slovensky Pohar"))
footnote_doc <- read_html('<html><table><tr><th>Home Away</th><th>ALP</th><th>BET</th><th>GAM</th></tr>
<tr><th>Alpha</th><td>-</td><td>0-3<sup>1</sup></td><td>1-0</td></tr>
<tr><th>Beta</th><td>2-0</td><td>-</td><td>0-0</td></tr>
<tr><th>Gamma</th><td>1-1</td><td>2-1</td><td>-</td></tr></table></html>')
stopifnot(wiki_games(footnote_doc)$games[Home=="Alpha" & Away=="Beta",Score]=="0-3")
round_w <- data.table(Home="Alpha",Away="Beta",Score="1-0",Stage=c("Matches 1-22","Matches 23-33"))
round_r <- data.table(RHome="Alpha",RAway="Beta",Score="1-0",RSSSFStage=c("Round 6","Round 25"),
  Date=as.Date(c("2022-11-08",NA)),RawLine="Alpha 1-0 Beta")
round_w <- rbind(round_w,data.table(Home="Alpha",Away="Beta",Score=paste0(2:6,"-0"),Stage="Matches 1-22"))
round_r <- rbind(round_r,data.table(RHome="Alpha",RAway="Beta",Score=paste0(2:6,"-0"),
  RSSSFStage=paste("Round",1:5),Date=as.Date("2022-10-01")+1:5,RawLine="supporting fixture"))
round_matched <- attach_dates(round_w,round_r)$games
stopifnot(round_matched$DateStatus[1]=="matched",is.na(round_matched$Date[2]))
contradictory_r <- copy(round_r); contradictory_r[3,RSSSFStage:="Round 30"]
stopifnot(attach_dates(round_w,contradictory_r)$games$DateStatus[1]=="ambiguous_repeated_fixture")
stopifnot(rsssf_date("May 24. Total Att: 151,872; Average: 18,984",2024L,2025L)==as.Date("2025-05-24"))
note_doc <- read_html('<html><pre>Round 1 [Apr 7]
Alpha 1-0 Beta
  [Scorer 21,
   Another 45]
Gamma 2-0 Delta
</pre></html>')
stopifnot(all(rsssf_games(note_doc,2024L,2024L)$Date==as.Date("2024-04-07")))
admin_doc <- read_html('<html><pre>Round 1 [Apr 7]
Alpha awd Beta [awarded 3-0]
Gamma 2-0 Delta
Epsilon ppd Zeta
Eta 1-1 Theta
Round 2 [Apr 14]
Alpha 0-0 Gamma
</pre></html>')
admin_games <- rsssf_games(admin_doc,2024L,2024L)
stopifnot(nrow(admin_games)==3L,
  identical(admin_games$Date,as.Date(c("2024-04-07","2024-04-07","2024-04-14"))))
stopifnot(is.na(rsssf_date("Aug 25,26",2012L,2013L)))
scorer_doc <- read_html('<html><pre>Round 1 [Aug 7]
Alpha 1-0 Beta
  [Ivanovski 65]
Gamma 2-0 Delta [in Elsewhere]
  [Markoski 68]
Epsilon 0-0 Zeta [behind closed doors]
[Aug 10,11]
Eta 1-0 Theta
[Aug 12]
Iota 1-0 Kappa [in Elsewhere; awarded]
15.08.24 Lambda 3-1 Mu
</pre></html>')
scorer_games <- rsssf_games(scorer_doc,2024L,2024L)
stopifnot(nrow(scorer_games)==6L,
  all(scorer_games$Date[1:3]==as.Date("2024-08-07")),
  all(is.na(scorer_games$Date[4:5])),scorer_games$Date[6]==as.Date("2024-08-15"),
  identical(scorer_games$RAway[2:3],c("Delta","Zeta")),
  !rsssf_date_heading("Glisic 7"),rsssf_date_heading("Sep 1,2"),
  rsssf_date("15.07.22",2022L,2023L)==as.Date("2022-07-15"),
  is.na(rsssf_date("15.07.21",2022L,2023L)))
detail_doc <- read_html('<html><pre>Alpha 1-0 Beta
15.07.22. Arena Stadium. Att: 1234.
Gamma 2-1 Delta
9.07 Vestur 27 6 6 15 30-65 24 Relegated
</pre></html>')
detail_games <- rsssf_games(detail_doc,2022L,2023L)
stopifnot(nrow(detail_games)==2L,all(is.na(detail_games$Date)),!rsssf_date_heading("2 1 0 1 6-3 3"))
nav_doc <- read_html('<html><pre><a href="#cup">Cyprus Cup</a>
Round 1 [Apr 7]
Alpha 1-0 Beta
Cyprus Cup 2024
Gamma 2-0 Delta
</pre></html>')
stopifnot(nrow(rsssf_games(nav_doc,2024L,2024L))==1L)
duplicate_w <- data.table(Home="Alpha",Away="Beta",Score="1-0",Stage="Results")
duplicate_r <- data.table(RHome="Alpha",RAway="Beta",Score="1-0",
  Date=as.Date(c("2024-01-01","2024-05-01")),RawLine="Alpha 1-0 Beta")
stopifnot(attach_dates(duplicate_w,duplicate_r)$games$DateStatus=="ambiguous_repeated_fixture")
twice <- attach_dates(rbind(duplicate_w,duplicate_w),duplicate_r)$games
stopifnot(all(twice$DateStatus=="matched"),identical(sort(twice$Date),sort(duplicate_r$Date)))
stopifnot(all(attach_dates(rbind(duplicate_w,duplicate_w),duplicate_r[1])$games$DateStatus=="ambiguous_repeated_fixture"))
extra_r <- rbind(duplicate_r,copy(duplicate_r[1])[,Date:=as.Date("2024-08-01")])
stopifnot(all(attach_dates(rbind(duplicate_w,duplicate_w),extra_r)$games$DateStatus=="ambiguous_repeated_fixture"))
undated_r <- rbind(duplicate_r,copy(duplicate_r[1])[,Date:=as.Date(NA)])
stopifnot(all(attach_dates(rbind(duplicate_w,duplicate_w),undated_r)$games$DateStatus=="ambiguous_repeated_fixture"))
stopifnot(all(attach_dates(rbind(duplicate_w,duplicate_w),rbind(duplicate_r,duplicate_r))$games$DateStatus=="matched"))
stopifnot(team_key("VPS (5th)")==team_key("VPS"),team_key("Schalke 04")!="schalke")
stopifnot(rsssf_date("24. VII. 2011.",2011L,2012L)==as.Date("2011-07-24"))
tail_doc <- read_html('<html><pre>1 . round (22. VII. 2011.)
Alpha - Beta 2:1 (24. VII. 2011.)
Gamma - Delta 1:0
</pre><h4>Hegelmann LFF Taur&#279; 2011</h4><pre>Round 1 [Aug 1]
Alpha 2-0 Gamma
</pre></html>')
tail_games <- rsssf_games(tail_doc,2011L,2012L)
stopifnot(nrow(tail_games)==2L,identical(tail_games$Date,as.Date(c("2011-07-24","2011-07-22"))),
  identical(tail_games$RAway,c("Beta","Delta")))
note_boundary_doc <- read_html('<html><pre>NB: Club demoted to
the third league (group South)
Round 1 [Apr 7]
Alpha 1-0 Beta
</pre></html>')
stopifnot(nrow(rsssf_games(note_boundary_doc,2024L,2024L))==1L)
link_doc <- read_html('<html><table><tr><td><a href="/wiki/Alpha_FC">Alpha</a></td>
<td><a href="./Alpha_FC">Alpha FC</a></td></tr></table></html>')
link_games <- data.table(Home=c("Alpha","Alpha FC"),Away="Beta",Score="1-0")
stopifnot(all(wiki_link_identities(link_games,link_doc)$Home=="Alpha FC"))
opponents <- data.table(Home="Alpha",Away="Alpha FC",Score="1-0")
stopifnot(identical(wiki_link_identities(opponents,link_doc)$Home,"Alpha"))
format_doc <- read_html('<html><pre>Round 1 [Aug 19]
Alpha 1-0 Beta
  {Scorer 20,
   Other 31]
Gamma 2-1 Pen-y-Bont
Alpha - - - 2-1
Alpha - Beta 2-0, 0-1
Anorthosis Famagusta 3-0 - - - -
Apollon Limassol - - - 1-1 -
Nea Salamina - 2-2 - - -
Cup 1/16 finals
Alpha - Beta 2-1
</pre></html>')
format_games <- rsssf_games(format_doc,2024L,2024L)
stopifnot(nrow(format_games)==2L,all(format_games$Date==as.Date('2024-08-19')),
  !any(format_games$Annotated),phase_key('Championship conference')=='championship')
slovak_cup <- read_html('<html><pre>Round 1 [Aug 19]
Alpha 1-0 Beta
Slovensk&#253; poh&#225;r
Gamma - Delta 2-1
</pre></html>')
stopifnot(nrow(rsssf_games(slovak_cup,2024L,2024L))==1L)
root <- "EuropeanFootball/pipeline_data"
stopifnot(rsssf_date("2010-JUL-24",2010L,2011L)==as.Date("2010-07-24"),
  rsssf_date("2010-07-24",2010L,2011L)==as.Date("2010-07-24"),
  is.na(rsssf_date("Aug 25,26",2012L,2013L)))
dash_doc <- read_html('<html><pre>Round 1 [Aug 4]\nAlpha 4&#8211;0 Beta\nGamma 1&#8722;2 Delta</pre></html>')
stopifnot(identical(rsssf_games(dash_doc,2014L,2015L)$Score,c("4-0","1-2")))
matrix_html <- '<table><tr><th>Home / Away</th><th>A</th><th>B</th><th>C</th></tr><tr><td>Alpha</td><td>-</td><td>2-0</td><td></td></tr><tr><td>Beta</td><td>1-0</td><td>-</td><td></td></tr><tr><td>Gamma</td><td></td><td></td><td>-</td></tr></table>'
nested_doc <- read_html(paste0('<html><table><tr><td>',matrix_html,'</td><td>',matrix_html,'</td></tr></table></html>'))
stopifnot(nrow(wiki_games(nested_doc)$games)==4L)
review_dir <- file.path(root,"Manual_Sources/Wikipedia_RSSSF_Alias_Audit/parser_checks")
dir.create(review_dir,recursive=TRUE,showWarnings=FALSE)
review_rows <- list()
summaries <- list()
cases <- data.table(Country=c("Iceland","Belarus","Cyprus","Russia","Albania","Croatia","Croatia","Finland","Finland","Lithuania"),
  Season=c("2024","2024","2007/08","2024/25","2024/25","2024/25","2011/12","2024","2011","2024"),
  Page=c("tablesi/ijs2024.html","tablesw/witr2024.html","tablesc/cyp08.html","tablesr/rus2025.html",
    "tablesa/alba2025.html","tablesk/kroa2025.html","tablesk/kroa2012.html","tablesf/fin2024.html","tablesf/fin2011.html","tablesl/lito2024.html"),
  Start=c(2024L,2024L,2007L,2024L,2024L,2024L,2011L,2024L,2011L,2024L),
  End=c(2024L,2024L,2008L,2025L,2025L,2025L,2012L,2024L,2011L,2024L))
cases <- rbind(cases,data.table(Country=c('Romania','Slovakia','Cyprus','Wales'),
  Season=c('1991/92','2007/08','2019/20','2024/25'),
  Page=c('tablesr/roem92.html','tabless/slow08.html','tablesc/cyp2020.html','tablesw/wal2025.html'),
  Start=c(1991L,2007L,2019L,2024L),End=c(1992L,2008L,2020L,2025L)))
cases <- rbind(cases,data.table(Country=c('Croatia','Romania','Kosovo','Faroe Islands','Wales','Russia','Moldova','Hungary','Albania'),
  Season=c('2010/11','2010/11','2012/13','2022','2010/11','2014/15','2013/14','2019/20','2018/19'),
  Page=c('tablesk/kroa2011.html','tablesr/roem2011.html','tablesk/kosovo2013.html','tablesf/far2022.html',
    'tablesw/wal2011.html','tablesr/rus2015.html','tablesm/mold2014.html','tablesh/hong2020.html','tablesa/alba2019.html'),
  Start=c(2010L,2010L,2012L,2022L,2010L,2014L,2013L,2019L,2018L),
  End=c(2011L,2011L,2013L,2022L,2011L,2015L,2014L,2020L,2019L)))
cases <- rbind(cases,data.table(Country=c('North Macedonia','North Macedonia','North Macedonia','Slovakia','Slovakia','Russia'),
  Season=c('2010/11','2011/12','2012/13','2018/19','2019/20','2022/23'),
  Page=c('tablesf/fyrom2011.html','tablesf/fyrom2012.html','tablesf/fyrom2013.html',
    'tabless/slow2019.html','tabless/slow2020.html','tablesr/rus2023.html'),
  Start=c(2010L,2011L,2012L,2018L,2019L,2022L),End=c(2011L,2012L,2013L,2019L,2020L,2023L)))
if(nzchar(Sys.getenv("PARSER_CHECK_CASE"))) cases <- cases[paste(Country,Season) %in% strsplit(Sys.getenv("PARSER_CHECK_CASE"),"|",fixed=TRUE)[[1L]]]
for(i in seq_len(nrow(cases))) {
  a <- cases[i]
  folder <- file.path(root,"Manual_Sources/Wikipedia_RSSSF_Alias_Audit",paste(a$Country,gsub("/","-",a$Season),sep="_"))
  w <- fread(file.path(folder,"all_wikipedia_games.csv"))
  saved_count <- sum(w$DateStatus=="matched")
  saved_total <- nrow(w)
  saved_fixture_keys <- sort(paste(w$Home,w$Away,w$Score,w$Stage,sep="|"))
  cache_name <- paste0(stringi::stri_replace_all_regex(URLdecode(w$SourcePage[1L]),"[^A-Za-z0-9._-]","_"),".html")
  cache_paths <- file.path(root,"Manual_Sources",c("Wikipedia_RSSSF_Alias_Audit/cache",
    "Wikipedia_RSSSF_Remaining_UEFA_Leagues/cache","Wikipedia_RSSSF_Four_Leagues/cache"),cache_name)
  cache_path <- cache_paths[file.exists(cache_paths)][1L]
  stopifnot(!is.na(cache_path))
  w <- wiki_games(read_html(cache_path))$games
  rsssf_paths <- c(file.path(root,"Source/rsssf/all/pages",a$Page),
    file.path(root,"Manual_Sources",c('Wikipedia_RSSSF_Alias_Audit/cache',
      'Wikipedia_RSSSF_Remaining_UEFA_Leagues/cache'),
      paste0('https___www.rsssf.org_',gsub('/','_',a$Page),'.html')))
  doc <- read_html(rsssf_paths[file.exists(rsssf_paths)][1L])
  r <- rsssf_games(doc,a$Start,a$End)
  result <- attach_dates(w,r)
  stopifnot(inherits(result$games$Date,"Date"),
    is.character(result$games$RSSSFLine),is.character(result$games$MatchMethod),
    all(!is.na(result$games[DateStatus=="matched",Date])))
  matched_dates <- result$games[DateStatus=="matched",Date]
  stopifnot(all(as.integer(format(matched_dates,"%Y")) %in% seq.int(a$Start,a$End)))
  corrected_count <- sum(result$games$DateStatus=="matched")
  fwrite(result$mapping,file.path(review_dir,paste0(a$Country,"_",gsub("/","-",a$Season),"_team_map.csv")))
  fwrite(r,file.path(review_dir,paste0(a$Country,"_",gsub("/","-",a$Season),"_rsssf_evidence.csv")))
  cat(a$Country,a$Season,"saved",saved_count,"corrected",corrected_count,"of",nrow(w),"\n")
  if(corrected_count<saved_count) {
    print(result$games[, .N,by=DateStatus])
    print(head(result$games[DateStatus!='matched',.(Home,Away,Score,DateStatus)],8L))
  }
  # A corrected Wikipedia score changes the matching problem even if the row
  # count is unchanged. Compare coverage monotonically only on identical input.
  same_fixtures <- identical(saved_fixture_keys,sort(paste(w$Home,w$Away,w$Score,w$Stage,sep="|")))
  if(same_fixtures) stopifnot(corrected_count>=saved_count)
  if(a$Country=='Albania' && a$Season=='2018/19') {
    stopifnot(!any(w$Score %in% c('0-31','3-01')),
      all(result$games[Home=='Kastrioti' & Away=='Kamza' & Score=='3-0',DateStatus]=='ambiguous_repeated_fixture'))
  }
  if(a$Country=='Faroe Islands' && a$Season=='2022') stopifnot(nrow(w)==135L,!any(grepl('Home.*Away',c(w$Home,w$Away))))
  summaries[[i]] <- data.table(Country=a$Country,Season=a$Season,PreviousDated=saved_count,
    DatedGames=corrected_count,PreviousWikipediaGames=saved_total,WikipediaGames=nrow(w),CoveragePercent=round(100*corrected_count/nrow(w),2))
  print(result$games[, .N,by=DateStatus])
  remaining <- copy(result$games[DateStatus!="matched"])
  meta_columns <- intersect(c("Country","Season"),names(remaining))
  if(length(meta_columns)) remaining[, (meta_columns):=NULL]
  remaining[, `:=`(Country=rep(a$Country,.N),Season=rep(a$Season,.N))]
  review_rows[[i]] <- remaining
  fwrite(result$mapping,file.path(review_dir,paste0(a$Country,"_",gsub("/","-",a$Season),"_team_map.csv")))
}
fwrite(rbindlist(review_rows,fill=TRUE),file.path(review_dir,"unresolved_examples.csv"))
fwrite(rbindlist(summaries),file.path(review_dir,"comparison.csv"))
cat("Review examples:",file.path(review_dir,"unresolved_examples.csv"),"\n")
tictoc::toc()
tryCatch(suppressWarnings(beepr::beep()),error=function(e) message("Checks finished; sound unavailable."))
