# Pure rebuild helper used by the one-off repair and subsequent weekly updates.
build_denmark_sources <- function(root) {
  library(data.table)
  base <- file.path(root, "EuropeanFootball/pipeline_data")
  report <- file.path(base, "Audit/denmark_source_rebuild")
  cache <- file.path(base, "Source/openfootball/denmark")
  dir.create(report, recursive=TRUE, showWarnings=FALSE)
  dir.create(cache, recursive=TRUE, showWarnings=FALSE)
  e <- new.env(parent=environment())
  wanted <- c("trim", "month_num", "infer_year_from_season", "result_code", "parse_comp_file")
  for (expr in parse(file.path(root, "scripts/EuropeanFootball/01_parse_openfootball.R"), encoding="UTF-8"))
    if (is.call(expr) && identical(expr[[1]], as.name("<-")) && as.character(expr[[2]])[1] %in% wanted) eval(expr,e)
  # Same committed Schochastics football-data snapshot used by other countries.
  raw <- as.data.table(nanoparquet::read_parquet(file.path(base, "Source/schochastics/games.parquet")))
  raw <- raw[tolower(competition)=="denmark" & tolower(level)=="national"]
  raw[, date:=as.Date(date)]
  # Preserve the existing project's historical scope: Superliga from 1991.
  raw <- raw[!is.na(date) & date>=as.Date("1991-01-01") & date<as.Date("2023-07-01")]
  raw[, start:=ifelse(as.integer(format(date,"%m"))>=7L, as.integer(format(date,"%Y")), as.integer(format(date,"%Y"))-1L)]
  raw[, Season:=ifelse(date<as.Date("1991-07-01"), "1991", sprintf("%d/%02d",start,(start+1L)%%100L))]
  reviews <- list(); accepted <- list()
  for (season in unique(raw$Season)) {
    z <- raw[Season==season]
    reasons <- character()
    if (anyNA(z[,.(date,home,away,gh,ga)]) || any(!nzchar(trimws(z$home)) | !nzchar(trimws(z$away)))) reasons<-c(reasons,"MISSING_FIELDS")
    if (any(z$gh<0 | z$ga<0 | z$gh!=floor(z$gh) | z$ga!=floor(z$ga),na.rm=TRUE)) reasons<-c(reasons,"INVALID_SCORE")
    if (any(z$home==z$away,na.rm=TRUE)) reasons<-c(reasons,"SELF_MATCH")
    if (anyDuplicated(z[,.(date,home,away)])) reasons<-c(reasons,"DUPLICATE_FIXTURE")
    td<-rbind(z[,.(date,team=home)],z[,.(date,team=away)])
    if (anyDuplicated(td)) reasons<-c(reasons,"TEAM_DOUBLE_BOOKED")
    dates<-table(z$date)
    if(nrow(z)>=50L && (length(dates)<5L || max(dates)/nrow(z)>=0.2)) reasons<-c(reasons,"PLACEHOLDER_DATES")
    reviews[[length(reviews)+1L]]<-data.table(Season=season,Source="schochastics",Matches=nrow(z),Status=if(length(reasons))"HELD" else "PASS",Reason=paste(reasons,collapse=";"))
    if(length(reasons))next
    accepted[[length(accepted)+1L]]<-z[,.(Season,Country="Denmark",Competition="danish_superliga",CompetitionType="league",Tier=1L,League="Danish Superliga",Date=as.character(date),Home=home,Away=away,Result=ifelse(gh>ga,"1-0",ifelse(gh<ga,"0-1","0.5-0.5")),Score=paste0(gh,"-",ga),Source="schochastics",SourcePage="https://github.com/schochastics/football-data",SourceFile=file.path(base,"Source/schochastics/games.parquet"))]
  }
  this_year<-as.integer(format(Sys.Date(),"%Y"))-as.integer(as.integer(format(Sys.Date(),"%m"))<7L)
  for(year in 2023:max(2024L,this_year)) {
    name<-sprintf("%d-%02d_dk1.txt",year,(year+1L)%%100L)
    path<-file.path(cache,name)
    weekly_cache<-file.path(cache,sprintf("%d-%02d",year,(year+1L)%%100L),"1-danish-superliga.txt")
    if(file.exists(weekly_cache) && !file.copy(weekly_cache,path,overwrite=TRUE))stop("Cannot read refreshed Denmark cache")
    previous<-file.path(base,"Audit/denmark_russia_source_repair/cache",name)
    if(!file.exists(path) && file.exists(previous))file.copy(previous,path)
    url<-paste0("https://raw.githubusercontent.com/openfootball/europe/master/denmark/",name)
    if(!file.exists(path)) {
      old_options<-options(timeout=max(120,getOption("timeout")))
      warnings<-character()
      err<-tryCatch(withCallingHandlers({download.file(url,paste0(path,".download"),mode="wb",quiet=TRUE);NULL},warning=function(w){warnings<<-c(warnings,conditionMessage(w));invokeRestart("muffleWarning")}),error=function(e)e)
      options(old_options)
      if(!is.null(err)) {
        if(year<=2024L || !any(grepl("404|Not Found",c(conditionMessage(err),warnings),ignore.case=TRUE)))stop(err)
        reviews[[length(reviews)+1L]]<-data.table(Season=sprintf("%d/%02d",year,(year+1L)%%100L),Source="openfootball",Matches=0L,Status="UNAVAILABLE",Reason="No OpenFootball top-flight file")
        next
      }
      if(!file.rename(paste0(path,".download"),path))stop("Cannot publish downloaded source")
    }
    z<-as.data.table(e$parse_comp_file(path,"Denmark","danish_superliga","league","Danish Superliga",1L))
    scored<-grepl("^[0-9]+-[0-9]+$",z$Score)
    if(!nrow(z) || anyNA(z$Date) || any(!scored & nzchar(z$Score)))stop("Invalid OpenFootball file: ",name)
    reviews[[length(reviews)+1L]]<-data.table(Season=z$Season[1],Source="openfootball",Matches=sum(scored),Status="PASS",Reason=paste(sum(!scored),"unscored fixtures excluded"))
    z<-z[scored];z[,`:=`(Source="openfootball",SourcePage=url,SourceFile=path)]
    accepted[[length(accepted)+1L]]<-z
  }
  fwrite(rbindlist(reviews,fill=TRUE),file.path(report,"coverage.csv"))
  result<-rbindlist(accepted,fill=TRUE)
  if(!nrow(result) || !all(c("openfootball","schochastics") %in% result$Source))stop("Both replacement sources must pass before removing old data")
  # Explicit source-name aliases keep existing club identities connected.
  mapping<-c("AaB"="Aalborg BK","AGF"="Aarhus GF","Akademisk Bk"="AB Gladsaxe","B 1903 Kobenhavn"="B 1903 K\u00f8benhavn","B 93 Koebenhavn"="B.93 K\u00f8benhavn","Bk Frem Kobenhavn"="BK Frem K\u00f8benhavn","Elite 3000 Helsingor"="FC Helsing\u00f8r","Esbjerg Fb"="Esbjerg","FC Hjorring"="Vendsyssel FF","Hb Koge"="HB K\u00f8ge","Herfolge Bk"="Herf\u00f8lge BK","Hobro Ik"="Hobro IK","Hvidovre If"="Hvidovre IF","Ikast Fs"="Ikast FS","K\u00f8benhavn"="Copenhagen","Koge Bk"="K\u00f8ge BK","Lyngby"="Lyngby BK","Naestved Bk"="N\u00e6stved BK","Odense Bk"="Odense","Vejle"="Vejle BK","Viborg"="Viborg FF")
  mapping<-c(mapping,"Farum"="Nordsj\u00e6lland","FC Vestsjalland"="FC Vestsj\u00e6lland")
  aliases<-fread(file.path(base,"Reference/team_aliases.csv"))[Country=="Denmark"]
  for(col in c("Home","Away")) {
    v<-result[[col]];i<-match(v,names(mapping));v[!is.na(i)]<-unname(mapping[i[!is.na(i)]])
    i<-match(v,aliases$SourceName);v[!is.na(i)]<-aliases$CanonicalName[i[!is.na(i)]]
    set(result,j=col,value=v)
  }
  if(anyDuplicated(result[,.(Date,Home,Away)]))stop("Duplicate replacement matches")
  result[]
}

replace_denmark_sources <- function(existing, replacement) {
  existing<-data.table::copy(data.table::as.data.table(existing))
  existing[,Date:=as.character(Date)]
  replacement<-data.table::copy(data.table::as.data.table(replacement))
  replacement[,Date:=as.character(Date)]
  keep<-!(existing$Country=="Denmark" & existing$CompetitionType=="league" & !is.na(existing$Tier) & existing$Tier==1L)
  result<-data.table::rbindlist(list(existing[keep],replacement),fill=TRUE)
  if(any(result$Country=="Denmark" & result$Source=="weltfussball_manual",na.rm=TRUE))stop("Old Denmark source remains outside top flight; inspect before proceeding")
  data.table::setorderv(result,c("Date","Country","Competition","Home","Away"))
  as.data.frame(result)
}
