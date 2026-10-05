# Season-local membership evidence only. Never use this to rewrite club identity.
rsssf_roster_abbreviation_match <- function(raw, roster) {
  tokens <- function(x, location=TRUE) {
    if(!location) x<-gsub("\\([^)]*\\)","",x)
    x<-tolower(stringi::stri_trans_general(x,"Latin-ASCII"))
    x<-gsub("['’]","",x)
    z<-strsplit(gsub("[^a-z0-9]+"," ",x),"[[:space:]]+")[[1]]
    z[nzchar(z) & !z %in% c("fc","sc","cf","fk","afc","al","el","xi")]
  }
  short<-tokens(raw)
  if(!length(short)) return(integer())
  matches<-function(full) {
    # Whole acronyms require at least three letters and an uppercase source.
    if(length(short)==1L && grepl("^[A-Z]{3,6}$",trimws(raw)) && length(full)>=3L &&
       identical(short,paste(substr(full,1,1),collapse=""))) return(TRUE)
    # Mixed names require at least one exact, substantive word. Remaining words
    # can expand into a unique initials group or a prefix of four+ characters.
    walk<-function(i,j,anchor) {
      if(i>length(short)) return(j>length(full) && anchor)
      if(j>length(full)) return(FALSE)
      a<-short[i]; b<-full[j]
      if(a==b && walk(i+1L,j+1L,anchor || nchar(a)>=4L)) return(TRUE)
      if(nchar(a)>=4L && nchar(b)>nchar(a) && startsWith(b,a) &&
         walk(i+1L,j+1L,anchor)) return(TRUE)
      if(nchar(a)>=2L && nchar(a)<=6L) {
        end<-j+nchar(a)-1L
        if(end<=length(full) && a==paste(substr(full[j:end],1,1),collapse="") &&
           walk(i+1L,end+1L,anchor)) return(TRUE)
      }
      FALSE
    }
    walk(1L,1L,FALSE)
  }
  which(vapply(roster,function(name)
    matches(tokens(name,FALSE)) || matches(tokens(name,TRUE)),logical(1)))
}
