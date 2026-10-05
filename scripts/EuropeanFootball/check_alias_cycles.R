suppressPackageStartupMessages(library(data.table))
a<-fread('EuropeanFootball/pipeline_data/Reference/team_aliases.csv',encoding='UTF-8')
map<-setNames(a$CanonicalName,paste(a$Country,a$SourceName,sep='|'))
found<-list()
for(i in seq_len(nrow(a))) {
 c<-a$Country[i]; n<-a$SourceName[i]; seen<-character()
 for(j in 1:40) {
 k<-paste(c,n,sep='|'); z<-unname(map[k])
 if(is.na(z)||z==n)break
 if(n %in% seen) {found[[length(found)+1L]]<-data.table(Country=c,SourceName=a$SourceName[i],Path=paste(c(seen,n),collapse=' -> '));break}
 seen<-c(seen,n);n<-z
 }
}
x<-if(length(found))rbindlist(found) else data.table()
print(x)
fwrite(x,'EuropeanFootball/pipeline_data/Manual_Sources/Team_Identity_Audit/systematic_split_review/source_validation/alias_cycles.csv')
