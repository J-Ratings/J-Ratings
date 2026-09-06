known_275 <- bind_rows(
  official_results %>%
    select(Date, Home, Away, Score) %>%
    mutate(SourceBlock = "official_255"),
  
  rsssf_date_candidates %>%
    select(Date, Home, Away, Score) %>%
    mutate(SourceBlock = "middle_6"),
  
  relegation_dates %>%
    inner_join(
      missing_wiki_results,
      by = c("Home", "Away")
    ) %>%
    select(Date, Home, Away, Score) %>%
    mutate(SourceBlock = "relegation_14")
) %>%
  distinct(
    Date,
    Home,
    Away,
    Score,
    .keep_all = TRUE
  )

print(
  data.frame(
    KnownRows = nrow(known_275)
  ),
  row.names = FALSE
)

known_counts <- known_275 %>%
  count(
    Home,
    Away,
    Score,
    name = "KnownN"
  )

wiki_counts <- wiki_results %>%
  count(
    Home,
    Away,
    Score,
    name = "WikiN"
  )

not_in_wiki <- known_counts %>%
  left_join(
    wiki_counts,
    by = c("Home", "Away", "Score")
  ) %>%
  mutate(
    WikiN = coalesce(WikiN, 0L),
    MissingN = KnownN - WikiN
  ) %>%
  filter(MissingN > 0L) %>%
  tidyr::uncount(MissingN) %>%
  select(
    Home,
    Away,
    Score
  )

print(
  not_in_wiki,
  n = Inf,
  row.names = FALSE
)

print(
  data.frame(
    MissingFromWikiObject = nrow(not_in_wiki)
  ),
  row.names = FALSE
)