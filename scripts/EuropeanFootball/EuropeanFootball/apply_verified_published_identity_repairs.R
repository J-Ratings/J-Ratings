# Explicit source-backed mappings, never a general fuzzy-name merge.
run_verified_identity_repairs <- function() {
  suppressPackageStartupMessages(library(data.table))
  root<-normalizePath(getwd(),winslash="/",mustWork=TRUE)
  base<-file.path(root,"EuropeanFootball/pipeline_data")
  path<-file.path(base,"Reference/team_aliases.csv")
  out<-file.path(base,"Manual_Sources/Team_Identity_Audit/systematic_split_review/verified_repairs")
  dir.create(out,recursive=TRUE,showWarnings=FALSE)
  cat("[1/4] Building explicit identity repairs...\n")
  updates<-fread(text="Country,SourceName,CanonicalName
Czechia,AC Sparta Praha,AC Sparta Praha
Czechia,Sparta Prague,AC Sparta Praha
Czechia,Sparta Praha,AC Sparta Praha
Czechia,Zlín,Zlín
Europe,AC Sparta Praha,AC Sparta Praha
Europe,Sparta Prague,AC Sparta Praha
Europe,Sparta Praha,AC Sparta Praha
Russia,CSKA,CSKA Moscow
Russia,Spartak,Spartak Moscow
Russia,Dinamo,Dynamo Moscow
Russia,Dinamo M,Dynamo Moscow
Russia,Dinamo Ms,Dynamo Moscow
Russia,Dinamo Mh,Dynamo Makhachkala
Russia,Akron,Akron Tolyatti
Russia,Pari NN,Pari Nizhny Novgorod
Russia,Pari NN (Nizh.Novgorod),Pari Nizhny Novgorod
Russia,Pari NN (N.Novgorod),Pari Nizhny Novgorod
Russia,Parí NN (Nížnij Nóvgorod),Pari Nizhny Novgorod
Russia,Ahmát (Gróznyj),Akhmat Grozny
Russia,FC Sóči,Sochi
Ukraine,Dynamo,Dynamo Kyiv
Ukraine,Shakhtar,Shakhtar Donetsk
Ukraine,Sachtar,Shakhtar Donetsk
Ukraine,Zorya,Zorya Luhansk
Ukraine,Zorja,Zorya Luhansk
Ukraine,Kolos,Kolos Kovalivka
Ukraine,Kryvbas,Kryvbas Kryvyi Rih
Ukraine,Metalist 1925,Metalist 1925 Kharkiv
Ukraine,Rukh,Rukh Lviv
Ukraine,Ruch,Rukh Lviv
Ukraine,Polissya,Polissya Zhytomyr
Ukraine,Polissja,Polissya Zhytomyr
Ukraine,Epitsentr,Epitsentr Kamianets-Podilskyi
Ukraine,Epicenter,Epitsentr Kamianets-Podilskyi
Ukraine,Karpaty,Karpaty Lviv
Ukraine,LNZ,LNZ Cherkasy
Uzbekistan,FC Andijon,Andijon
Uzbekistan,Buxoro,Bukhara
Uzbekistan,FC Buxoro,Bukhara
Uzbekistan,Xorazm,Khorazm
Uzbekistan,Xorazm (Urganch),Khorazm
Uzbekistan,Qo'qon 1912,Kokand 1912
Uzbekistan,Qo'qon-1912,Kokand 1912
Uzbekistan,FC Qo'qon-1912,Kokand 1912
Uzbekistan,Mash'al (Muborak),Mash'al
Uzbekistan,Metallurg (Bekobod),Metallurg
Uzbekistan,Sho'rtan (G'uzor),Sho'rtan
Uzbekistan,Olimpik (Toshkent),Olimpik
Uzbekistan,Paxtakor,Pakhtakor
Uzbekistan,So'g'diyona,Sogdiana
Uzbekistan,Surxon,Surkhon
Uzbekistan,OKMK,AGMK",encoding="UTF-8")
  updates[,Evidence:=fifelse(Country %chin% c("Czechia","Europe"),
    "Existing reciprocal aliases; choose one stable identity",
    fifelse(Country=="Russia","RSSSF rus2025 standings and short fixture labels",
    fifelse(Country=="Ukraine","RSSSF oekr2025/oekr2026 standings and fixture labels",
    "RSSSF oez2024/oez2025 standings; supplied Wikipedia names CSV")))]
  original<-fread(path,encoding="UTF-8")
  new<-copy(original)
  for(i in seq_len(nrow(updates))) {
    u<-updates[i];idx<-which(new$Country==u$Country&new$SourceName==u$SourceName)
    if(length(idx))new[idx,CanonicalName:=u$CanonicalName] else new<-rbind(new,u[,.(Country,SourceName,CanonicalName)])
  }
  cat("[2/4] Checking conflicts, cycles and affected recorded names...\n")
  if(nrow(new[,.(N=uniqueN(CanonicalName)),by=.(Country,SourceName)][N>1L]))stop("Conflicting alias targets.")
  amap<-setNames(new$CanonicalName,paste(new$Country,new$SourceName,sep="|"))
  resolve<-function(c,n) {
    seen<-character()
    for(j in seq_len(50L)) {
      if(n %in% seen)stop("Alias cycle: ",c," / ",paste(c(seen,n),collapse=" -> "))
      seen<-c(seen,n);z<-unname(amap[paste(c,n,sep="|")])
      if(is.na(z)||z==n)return(n)
      n<-z
    };stop("Alias chain too long.")
  }
  invisible(mapply(resolve,new$Country,new$SourceName,USE.NAMES=FALSE))
  inventory<-fread(file.path(dirname(out),"club_inventory.csv"),encoding="UTF-8")
  inventory[,ResolvedName:=mapply(resolve,Country,Name,USE.NAMES=FALSE)]
  impact<-inventory[Name!=ResolvedName]
  fwrite(impact,file.path(out,"affected_recorded_names.csv"),na="")
  fwrite(updates,file.path(out,"explicit_alias_changes.csv"),na="")
  # Existing audit head-to-head evidence is an independent veto on new merges.
  pairs<-fread(file.path(dirname(out),"source_validation/identity_validation.csv"),encoding="UTF-8")
  pairs[,`:=`(ResolvedA=mapply(resolve,CountryA,NameA,USE.NAMES=FALSE),
              ResolvedB=mapply(resolve,CountryB,NameB,USE.NAMES=FALSE))]
  conflict<-pairs[CountryA==CountryB&ResolvedA==ResolvedB&HeadToHeadMatches>0L]
  if(nrow(conflict)) {fwrite(conflict,file.path(out,"merge_conflicts.csv"));stop("Proposed identities have head-to-head fixtures; no aliases written.")}
  # Check every master row, not just the pairs caught by the earlier audit.
  m<-fread(file.path(base,"Matches_Clean_Combined/european_football_all_matches.csv"),
    select=c("Country","Home","Away","HomeAssociation","AwayAssociation","CompetitionType","Date","Score"),encoding="UTF-8")
  hc<-m$Country;ac<-m$Country
  hi<-which(m$CompetitionType=="continental" & !is.na(m$HomeAssociation)&nzchar(m$HomeAssociation))
  ai<-which(m$CompetitionType=="continental" & !is.na(m$AwayAssociation)&nzchar(m$AwayAssociation))
  hc[hi]<-m$HomeAssociation[hi];ac[ai]<-m$AwayAssociation[ai]
  distinct<-unique(rbind(data.table(Country=hc,Name=m$Home),data.table(Country=ac,Name=m$Away)))
  distinct<-distinct[!is.na(Country)&!is.na(Name)&nzchar(Name)]
  distinct[,Resolved:=mapply(resolve,Country,Name,USE.NAMES=FALSE)]
  lookup<-setNames(distinct$Resolved,paste(distinct$Country,distinct$Name,sep="|"))
  hr<-unname(lookup[paste(hc,m$Home,sep="|")]);ar<-unname(lookup[paste(ac,m$Away,sep="|")])
  touched<-paste(hc,m$Home,sep="|") %chin% paste(updates$Country,updates$SourceName,sep="|") |
    paste(ac,m$Away,sep="|") %chin% paste(updates$Country,updates$SourceName,sep="|")
  self<-which(touched&hc==ac&!is.na(hr)&!is.na(ar)&hr==ar&m$Home!=m$Away)
  if(length(self)) {fwrite(m[self],file.path(out,"self_match_conflicts.csv"));stop("A new alias would make opponents the same club; aliases unchanged.")}
  cat("  Complete-master opponent check passed:",nrow(m),"matches\n")
  cat("[3/4] Proposed changes:",nrow(updates),"explicit mappings\n")
  print(impact[,.(RecordedNames=.N,Appearances=sum(Appearances)),by=Country])
  if(Sys.getenv("APPLY_VERIFIED_IDENTITY_REPAIRS")!="1") {
    cat("Preview only. Set APPLY_VERIFIED_IDENTITY_REPAIRS=1 to apply. Report:",out,"\n");return(invisible(NULL))
  }
  cat("[4/4] Backing up and writing aliases...\n")
  stamp<-format(Sys.time(),"%Y%m%d_%H%M%S")
  backup<-sub("\\.csv$",paste0("_before_verified_identity_repairs_",stamp,".csv"),path)
  if(!file.copy(path,backup,overwrite=FALSE))stop("Backup failed.")
  temporary<-paste0(path,".identity.tmp");fwrite(new,temporary,na="")
  readback<-fread(temporary,encoding="UTF-8")
  if(!identical(new,readback))stop("Alias CSV verification failed; original unchanged.")
  if(!file.copy(temporary,path,overwrite=TRUE))stop("Alias write failed; backup retained.")
  unlink(temporary)
  cat("Aliases updated. Backup:",backup,"\nMaster matches unchanged. Report:",out,"\n")
}
run_verified_identity_repairs()
