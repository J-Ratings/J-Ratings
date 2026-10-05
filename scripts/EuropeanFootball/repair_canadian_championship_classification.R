# Revalidate saved flags against cached source blocks; preview by default.
run_canadian_cup_repair <- function() {
  suppressPackageStartupMessages(library(data.table))
  on.exit(if(interactive()&&requireNamespace("beepr",quietly=TRUE))try(beepr::beep(),silent=TRUE),add=TRUE)
  base<-file.path(normalizePath(getwd(),winslash="/",mustWork=TRUE),"EuropeanFootball/pipeline_data")
  out<-file.path(base,"Manual_Sources/Team_Identity_Audit/systematic_split_review/canadian_repair")
  dir.create(out,recursive=TRUE,showWarnings=FALSE)
  path<-file.path(base,"Matches_Clean_Combined/european_football_all_matches.csv")
  cat("[1/4] Reading master and saved Canadian source flags...\n")
  m<-fread(path,encoding="UTF-8");old_names<-names(m)
  flags<-fread(file.path(dirname(out),"source_validation/suspect_league_source_sections.csv"),encoding="UTF-8")
  flags<-flags[Country=="Canada" & grepl("Canadian Championship",Sections,ignore.case=TRUE)]
  norm<-function(x) trimws(gsub(" +"," ",gsub("[^a-z0-9 ]"," ",tolower(stringi::stri_trans_general(x,"Latin-ASCII")))))
  decisions<-list();approved<-integer()
  cat("[2/4] Revalidating source sections and ambiguity...\n")
  files<-unique(flags$SourceFile)
  for(fi in seq_along(files)) {
    f<-files[fi];fs<-flags[SourceFile==f]
    doc<-xml2::read_html(f)
    blocks<-xml2::xml_find_all(doc,"//pre")
    lines<-list()
    for(bi in seq_along(blocks)) {
      heading<-xml2::xml_text(xml2::xml_find_first(blocks[[bi]],"preceding::*[self::h1 or self::h2 or self::h3 or self::h4 or self::h5 or self::h6][1]"))
      tx<-strsplit(xml2::xml_text(blocks[[bi]]),"\n",fixed=TRUE)[[1]]
      lines[[bi]]<-data.table(Heading=heading,Line=trimws(tx),Norm=norm(tx))
    }
    all<-rbindlist(lines)
    for(i in seq_len(nrow(fs))) {
      rr<-fs[i]
      # Do not trust saved row numbers after any intervening master rewrite.
      ids<-which(m$Country==rr$Country & m$Season==rr$Season & m$Date==rr$Date &
        m$Home==rr$Home & m$Away==rr$Away & m$Score==rr$Score & m$SourceFile==f)
      hits<-all[grepl(norm(rr$Home),Norm,fixed=TRUE)&grepl(norm(rr$Away),Norm,fixed=TRUE)&
        grepl(gsub(" ","",rr$Score,fixed=TRUE),gsub(" ","",Line,fixed=TRUE),fixed=TRUE)]
      cup<-nrow(hits)>0L && all(!is.na(hits$Heading)&grepl("Canadian Championship",hits$Heading,ignore.case=TRUE))
      status<-if(!length(ids))"NOT_FOUND_OR_ALREADY_CHANGED" else if(!cup)"AMBIGUOUS_SOURCE_SECTION_HELD" else "VERIFIED_CUP_RECLASSIFICATION"
      if(cup&&length(ids))approved<-c(approved,ids[m$CompetitionType[ids]=="league"])
      rr[,Decision:=NULL]
      decisions[[length(decisions)+1L]]<-data.table(rr,Decision=status,SourceHeadings=paste(unique(hits$Heading),collapse="; "))
    }
    cat("  Pages checked:",fi,"/",length(files),"\n")
  }
  review<-rbindlist(decisions);fwrite(review,file.path(out,"source_verification.csv"),na="")
  approved<-unique(approved)
  preview<-copy(m[approved]);fwrite(preview,file.path(out,"before_rows.csv"),na="")
  cat("[3/4] Proposed classification repairs:",length(approved),"rows\n")
  print(review[,.(Rows=.N),by=Decision])
  if(!length(approved)||Sys.getenv("APPLY_CANADIAN_CUP_REPAIR")!="1") {
    cat("Preview only. Set APPLY_CANADIAN_CUP_REPAIR=1 to apply. Report:",out,"\n");return(invisible(NULL))
  }
  m[approved,`:=`(Competition="canadian_championship",CompetitionType="domestic_cup",
    Tier=NA_integer_,League="Canadian Championship")]
  # Only classification fields may change. No name/date/score edits or removals.
  unchanged<-setdiff(old_names,c("Competition","CompetitionType","Tier","League"))
  if(!identical(m[approved,..unchanged],preview[,..unchanged]))stop("Unexpected match-data changes.")
  dupcols<-c("Country","Date","Home","Away","Score","Competition","CompetitionType")
  before_dups<-sum(duplicated(fread(path,select=dupcols,encoding="UTF-8")))
  if(sum(duplicated(m[,..dupcols]))>before_dups)stop("Repair would introduce duplicate fixture keys; master unchanged.")
  if(any(grepl(intToUtf8(34),m$Home,fixed=TRUE)|grepl(intToUtf8(34),m$Away,fixed=TRUE)))
    stop("Quoted team names require repair before a master rewrite.")
  cat("[4/4] Backing up and safely writing master...\n")
  backup<-sub("\\.csv$",paste0("_before_canadian_cup_repair_",format(Sys.time(),"%Y%m%d_%H%M%S"),".csv"),path)
  if(!file.copy(path,backup,overwrite=FALSE))stop("Backup failed.")
  temp<-paste0(path,".canadian_repair.tmp");fwrite(m,temp,na="")
  checked<-fread(temp,encoding="UTF-8")
  if(nrow(checked)!=nrow(m)||!identical(names(checked),old_names)||
     !identical(checked$Home,m$Home)||!identical(checked$Away,m$Away))stop("CSV verification failed; master unchanged.")
  if(!file.copy(temp,path,overwrite=TRUE))stop("Master write failed; backup retained.")
  unlink(temp)
  fwrite(m[approved],file.path(out,"after_rows.csv"),na="")
  cat("Master updated; rows unchanged:",nrow(m),". Reclassified:",length(approved),"\nBackup:",backup,"\n")
}
run_canadian_cup_repair()
