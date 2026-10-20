# Adapt legacy BR-separated cells to the modern source token structure in memory.
# Saved HTML is never modified; table spans and SGF links remain source evidence.
go_tournament_document <- function(path) {
  doc <- xml2::read_html(path)
  score <- "^[BW]\\+(R|T|[0-9]+(\\.[0-9]+)?)$"
  for(table in xml2::xml_find_all(doc,"//table[not(ancestor::table)]")) {
    if(length(xml2::xml_find_all(table,".//span[contains(@class,'tournament-result-item')]"))) next
    heading <- trimws(xml2::xml_text(xml2::xml_find_first(table,"preceding::*[self::h3][1]")))
    headers <- trimws(xml2::xml_text(xml2::xml_find_all(table,"./tr[1]/th|./thead/tr[1]/th|./tbody/tr[1]/th")))
    cls <- xml2::xml_attr(table,"class"); if(is.na(cls))cls <- ""
    # Swiss cells identify opponents by number; they are not league matrices.
    if(grepl("swiss",cls,ignore.case=TRUE))next
    if(grepl("title match",heading,ignore.case=TRUE) && "Players" %in% headers)
      xml2::xml_set_attr(table,"class","title-legacy")
    else if("Players" %in% headers && "Rank" %in% headers)
      xml2::xml_set_attr(table,"class","league-legacy")
    else if("Players" %in% headers && any(grepl("round|final",headers,ignore.case=TRUE)))
      xml2::xml_set_attr(table,"class","tournament-legacy")
    else next
    for(cell in xml2::xml_find_all(table,"./tr/td|./tr/th|./tbody/tr/td|./tbody/tr/th|./thead/tr/th")) {
      # Line breaks separate names, results, dates and series game headings.
      breaks <- xml2::xml_find_all(cell,".//br")
      if(!length(breaks) && !grepl(score,trimws(xml2::xml_text(cell))))next
      kifu_nodes <- xml2::xml_find_all(cell,".//a[contains(@href,'/kifu/')]")
      kifu <- xml2::xml_attr(kifu_nodes,"href")
      kifu_results <- trimws(xml2::xml_text(kifu_nodes))
      for(br in breaks)xml2::xml_set_text(br,"\n")
      tokens <- trimws(strsplit(xml2::xml_text(cell),"\n",fixed=TRUE)[[1L]])
      tokens <- tokens[nzchar(tokens)]
      if(!length(tokens))next
      xml2::xml_remove(xml2::xml_contents(cell))
      add <- function(parent,token) {
        s <- xml2::xml_add_child(parent,"span",class="tournament-result-item")
        hits <- which(kifu_results==token)
        if(grepl(score,token) && length(hits)==1L) {
          a <- xml2::xml_add_child(s,"a",href=kifu[hits]); xml2::xml_set_text(a,token)
        } else xml2::xml_set_text(s,token)
      }
      scores <- which(grepl(score,tokens))
      if(length(scores)>1L) {
        # Each explicit winner/result pair is a separate series game.
        for(i in seq_along(scores)) {
          at <- scores[i]; if(at<2L)next
          m <- xml2::xml_add_child(cell,"span",class="match")
          add(m,paste("Game",i)); add(m,tokens[at-1L]); add(m,tokens[at])
          if(at<length(tokens) && grepl("^([0-9]{4}-)?[0-9]{2}-[0-9]{2}$",tokens[at+1L]))add(m,tokens[at+1L])
        }
      } else for(token in tokens)add(cell,token)
    }
  }
  doc
}
