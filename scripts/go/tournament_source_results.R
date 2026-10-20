# Parse saved source results without the historical coverage-audit pipeline.
# Unknown pairings/dates are retained as review items, never guessed/imported.
go_source_results <- function(path, url, family, calendar, person) {
  library(xml2); library(data.table)
  source("scripts/go/tournament_source_document.R")
  doc <- go_tournament_document(path)
  text <- function(n) trimws(gsub("[[:space:]]+"," ",xml_text(n)))
  items <- function(n) trimws(xml_text(xml_find_all(n,".//span[contains(@class,'tournament-result-item')]")))
  clean <- function(s) trimws(sub("\\([^)]*\\)\\s*$","",sub("^\\([a-z]+\\)","",s)))
  score_rx <- "^[BW]\\+(R|T|[0-9]+(\\.[0-9]+)?)$"
  rows <- list(); review <- list()
  date_of <- function(tokens) {
    full <- unique(tokens[grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$",tokens)])
    if(length(full)==1L)return(full)
    md <- unique(tokens[grepl("^[0-9]{2}-[0-9]{2}$",tokens)])
    days <- unique(calendar[Link==sub("https://gotoeveryone.k2ss.info","",url,fixed=TRUE) & substr(Date,6,10) %in% md,Date])
    if(length(days)==1L)days else ""
  }
  emit <- function(a,b,winner,result,tokens,stage,round,node) {
    a <- person(clean(a));b <- person(clean(b));w <- person(clean(winner))
    if(!nzchar(a$name)||!nzchar(b$name)||a$name==b$name||!w$name %in% c(a$name,b$name)) {
      review[[length(review)+1L]] <<- list(stage=stage,round=round,reason="Unresolved pairing",sourceText=text(node));return()
    }
    kifu <- xml_attr(xml_find_first(node,".//a[contains(@href,'/kifu/')]"),"href")
    rows[[length(rows)+1L]] <<- data.table(SourceURL=url,Family=family,Stage=stage,Round=round,
      Date=date_of(tokens),PlayerA=a$name,PlayerB=b$name,Winner=w$name,CanonicalA=a$name,
      CanonicalB=b$name,CanonicalWinner=w$name,Result=result,KifuLink=if(is.na(kifu)) "" else kifu)
  }
  for(row in xml_find_all(doc,"//li[contains(concat(' ',normalize-space(@class),' '),' table-row ')]")) {
    nodes <- xml_find_all(row,"./div|./span");values <- vapply(as.list(nodes),text,"")
    si <- which(grepl(score_rx,values));if(length(si)!=1L)next
    p <- which(grepl("al_name|kl_name|player",xml_attr(nodes,"class")))
    if(length(p)!=2L)next
    style <- regmatches(xml_attr(nodes[[si]],"class"),regexpr("back[0-9]+",xml_attr(nodes[[si]],"class")))
    if(!length(style)||!nzchar(style))next
    wi <- p[grepl(paste0("(^| )",style,"( |$)"),xml_attr(nodes[p],"class"))]
    if(length(wi)!=1L)next
    heading <- text(xml_find_first(row,"preceding::*[self::h4][1]"))
    dates <- regmatches(heading,gregexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}",heading))[[1L]]
    emit(values[p[1]],values[p[2]],values[wi],values[si],c(values,dates),
      text(xml_find_first(row,"preceding::*[self::h3][1]")),heading,row)
  }
  for(table in xml_find_all(doc,"//table[not(ancestor::table)]")) {
    trs <- xml_find_all(table,"./tr|./thead/tr|./tbody/tr|./tfoot/tr")
    grid <- matrix(NA_integer_,length(trs),64L);cells <- list();valid <- TRUE
    for(r in seq_along(trs)) {
      col <- 1L
      for(n in xml_find_all(trs[[r]],"./td|./th")) {
        while(col<=64L&&!is.na(grid[r,col]))col <- col+1L
        span <- function(a){v <- suppressWarnings(as.integer(xml_attr(n,a)));if(is.na(v))1L else v}
        rs <- span("rowspan");cs <- span("colspan")
        if(rs<1L||cs<1L||col+cs-1L>64L||r+rs-1L>nrow(grid)){valid <- FALSE;break}
        cells[[length(cells)+1L]] <- list(node=n,row=r,col=col,rs=rs,text=text(n),items=items(n),header=xml_name(n)=="th")
        grid[r:(r+rs-1L),col:(col+cs-1L)] <- length(cells);col <- col+cs
      }
      if(!valid)break
    }
    if(!valid){review[[length(review)+1L]] <- list(reason="Invalid table spans");next}
    stage <- text(xml_find_first(table,"preceding::*[self::h3][1]"))
    class <- xml_attr(table,"class");if(is.na(class))class <- ""
      row_person <- function(r) {
      ids <- unique(na.omit(grid[r,1:2]))
      for(id in ids){c <- cells[[id]];if(!c$header&&!length(c$items)&&nzchar(c$text)&&!grepl("^[0-9-]+$",c$text))return(list(name=clean(c$text),col=c$col))}
      list(name="",col=0L)
    }
    table_players <- lapply(seq_len(nrow(grid)),row_person)
    table_players <- table_players[vapply(table_players,function(p)nzchar(p$name),TRUE)]
    for(cell in cells) {
      if(cell$header||length(xml_find_all(cell$node,".//table")))next
      match_nodes <- xml_find_all(cell$node,".//span[contains(concat(' ',normalize-space(@class),' '),' match ')]")
      if(!length(match_nodes))match_nodes <- list(cell$node)
      hs <- Filter(function(id)cells[[id]]$header,unique(na.omit(grid[,cell$col])))
      above <- Filter(function(id)cells[[id]]$row<cell$row,hs)
      header <- if(length(above))cells[[tail(above,1L)]] else list(text="",items=character())
      round <- if(length(header$items))header$items[1L] else header$text
      for(mn in match_nodes) {
        its <- items(mn);scores <- its[grepl(score_rx,its)]
        if(!length(scores)&&grepl(score_rx,text(mn)))scores <- text(mn)
        if(length(scores)!=1L)next
        result <- scores[1L];a <- b <- winner <- ""
        if(startsWith(class,"title-")) {
          p <- row_person(cell$row);pn <- unique(vapply(table_players,function(p)p$name,""))
          if(length(pn)==2L&&p$name %in% pn){a <- winner <- p$name;b <- setdiff(pn,a)}
        } else if(startsWith(class,"league-")) {
          # Only the winning row stores a coloured result; ignore its mirror.
          cl <- xml_attr(cell$node,"class")
          if(is.na(cl)||grepl("(^| )back0( |$)",cl)||!grepl("back[1-9]",cl))next
          p <- row_person(cell$row);index <- cell$col-p$col
          if(index>=1L&&index<=length(table_players)){a <- winner <- p$name;b <- table_players[[index]]$name}
        } else if(cell$col>1L) {
          prior <- unique(na.omit(grid[cell$row:(cell$row+cell$rs-1L),cell$col-1L]))
          names <- vapply(prior,function(id){c <- cells[[id]];clean(if(length(c$items))c$items[1L] else c$text)},"")
          if(length(names)==2L){a <- names[1L];b <- names[2L]}
          ix <- match(result,its);if(!is.na(ix)&&ix>1L)winner <- clean(its[ix-1L])
        }
        own_dates <- its[grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$|^[0-9]{2}-[0-9]{2}$",its)]
        emit(a,b,winner,result,if(length(own_dates))own_dates else header$items,stage,round,mn)
      }
    }
  }
  empty <- data.table(SourceURL=character(),Family=character(),Stage=character(),Round=character(),Date=character(),PlayerA=character(),PlayerB=character(),Winner=character(),CanonicalA=character(),CanonicalB=character(),CanonicalWinner=character(),Result=character(),KifuLink=character())
  result <- if(length(rows))unique(rbindlist(rows),by=c("Stage","Round","Date","PlayerA","PlayerB","Winner","Result")) else empty
  list(rows=result,review=review)
}
