# Country identifies the club; competition Country may identify its league host.
canadian_mls_identity <- function(name, country) {
  key <- tolower(stringi::stri_trans_general(trimws(name), "Latin-ASCII"))
  canonical <- c("toronto fc"="Toronto FC", "vancouver whitecaps"="Vancouver Whitecaps FC",
    "vancouver whitecaps fc"="Vancouver Whitecaps FC", "cf montreal"="CF Montréal",
    "montreal cf"="CF Montréal", "montreal impact"="CF Montréal", "impact montreal"="CF Montréal")
  hit <- country %in% c("Canada","United States","North America","Europe","Asia",
                       "Africa","South America","Oceania","World") & key %in% names(canonical)
  name[hit] <- unname(canonical[key[hit]])
  name
}
club_association <- function(country, name) {
  key <- canadian_mls_identity(name,country)
  use <- country %in% c("Canada","United States","North America","Europe","Asia",
                       "Africa","South America","Oceania","World") &
    key %in% c("Toronto FC","Vancouver Whitecaps FC","CF Montréal")
  country[use] <- "Canada"
  country
}
resolve_alias_chain <- function(name,country,local_map,regional_map) {
  original <- name
  for(step in seq_len(30L)) {
    next_name <- name
    hit <- unname(local_map[paste(country,name,sep="\r")])
    regional <- country %in% c("Europe","Asia","Africa","North America","South America","Oceania")
    fallback <- unname(regional_map[name])
    use_fallback <- regional & is.na(hit) & !is.na(fallback)
    hit[use_fallback] <- fallback[use_fallback]
    use <- !is.na(hit)&hit!=name
    # Chester FC and Chester City require historical identity review. Preserve
    # the pre-existing single lookup rather than extending this suspect chain.
    if(step>1L)use[original=="Chester FC"&country=="England"]<-FALSE
    if(!any(use)) return(canadian_mls_identity(name,country))
    next_name[use]<-hit[use]
    name<-next_name
  }
  stop("Team alias cycle or chain longer than 30 steps; check team_aliases.csv.")
}
