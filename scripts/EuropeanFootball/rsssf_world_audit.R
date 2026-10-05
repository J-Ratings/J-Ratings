# RSSSF-only audit of non-UEFA domestic top flights and confederation club cups.
# Cached HTML only: no downloads, no pauses, and no production changes.
#
# Run from anywhere:
# source("C:/Users/stjuk/Documents/GitHub/J-Ratings/scripts/EuropeanFootball/rsssf_world_audit.R")
#
# Defaults to 2010-2025 to keep the first worldwide run manageable.
# Optional filters:
# Sys.setenv(WORLD_AUDIT_MIN_YEAR="2000", WORLD_AUDIT_MAX_YEAR="2025")
# Sys.setenv(WORLD_AUDIT_CONFEDERATIONS="AFC,CAF,CONCACAF,CONMEBOL,OFC")
# Sys.setenv(WORLD_AUDIT_COUNTRIES="Japan,Brazil,New Zealand")

root <- normalizePath(
  Sys.getenv("J_RATINGS_REPO", "C:/Users/stjuk/Documents/GitHub/J-Ratings"),
  winslash = "/", mustWork = TRUE
)
required <- c("data.table", "xml2")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Install these packages first: ", paste(missing, collapse = ", "))

index_file <- file.path(
  root, "EuropeanFootball/pipeline_data/Source/rsssf/uefa_raw/pages/curdom.html"
)
if (!file.exists(index_file)) stop("Missing cached RSSSF world index: ", index_file)
doc <- xml2::read_html(index_file)

section_links <- function(index_href, region) {
  anchor <- xml2::xml_find_first(doc, paste0("//a[@href='", index_href, "']"))
  if (inherits(anchor, "xml_missing")) stop("Missing index section: ", index_href)
  para <- xml2::xml_find_first(anchor, "following::p[1]")
  links <- xml2::xml_find_all(para, ".//a[contains(@href,'.html')]")
  data.table::data.table(
    Country = trimws(xml2::xml_text(links)),
    CurrentHref = xml2::xml_attr(links, "href"),
    Region = region
  )
}

current <- data.table::rbindlist(list(
  section_links("results-afr.html", "Africa"),
  section_links("results-ame.html", "Americas"),
  section_links("results-aso.html", "Asia/Oceania")
))
current <- current[nzchar(Country) & !grepl("women|futsal|youth", CurrentHref, ignore.case = TRUE)]

# Assign federation membership from the same reference used by the Elo model.
# Geographic inference misclassified several unaffiliated territories (for
# example Falkland Islands as CONCACAF and Kiribati as AFC) and Tuvalu as AFC.
country_alias <- c(
  "Hongkong" = "Hong Kong", "Macao" = "Macau", "East Timor" = "Timor-Leste",
  "Congo-Brazzaville" = "Congo", "Congo-Kinshasa" = "DR Congo",
  "Guinea Bissau" = "Guinea-Bissau", "French Guyana" = "French Guiana",
  "US Virgin Islands" = "United States Virgin Islands", "Surinam" = "Suriname",
  "Fiji (districts)" = "Fiji", "Fiji (national)" = "Fiji",
  "Vanuatu (PVFL)" = "Vanuatu", "Vanuatu (VFFCL)" = "Vanuatu",
  "Central African Republic (Bangui)" = "Central African Republic"
)
current[Country %chin% names(country_alias), Country := unname(country_alias[Country])]
current[, Country := sub(" \\((districts|national)\\)$", "", Country, ignore.case = TRUE)]

seed_path <- file.path(root, "EuropeanFootball/pipeline_data/Reference/non_uefa_country_seeds.csv")
if (!file.exists(seed_path)) stop("Missing federation reference: ", seed_path)
association_map <- unique(data.table::fread(seed_path, encoding = "UTF-8")[
  suppressWarnings(as.integer(Tier)) == 1L & Confederation != "UEFA",
  .(Country, Confederation)
])
ambiguous_associations <- association_map[, .(N = data.table::uniqueN(Confederation)), by = Country][N > 1L]
if (nrow(ambiguous_associations)) stop("Federation reference assigns a country to multiple confederations.")
unaffiliated <- current[!Country %chin% association_map$Country, unique(Country)]
if (length(unaffiliated)) {
  message("Excluded unaffiliated/non-model RSSSF associations: ", paste(sort(unaffiliated), collapse = ", "))
}
current <- merge(current, association_map, by = "Country", all = FALSE)

requested_confeds <- trimws(strsplit(Sys.getenv(
  "WORLD_AUDIT_CONFEDERATIONS", "AFC,CAF,CONCACAF,CONMEBOL,OFC"
), ",", fixed = TRUE)[[1L]])
current <- current[Confederation %in% requested_confeds]

current[, HrefNoFragment := sub("#.*$", "", CurrentHref)]
current[, Directory := dirname(HrefNoFragment)]
current[, CurrentFile := basename(HrefNoFragment)]
current[, Prefix := sub("(?:19|20)?[0-9]{2}[a-z]?[.]html$", "", CurrentFile, perl = TRUE)]
regex_escape <- function(x) gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)
current[, FilePattern := paste0(
  "^", regex_escape(Prefix), "(?:[0-9]{2}|[0-9]{4})[a-z]?[.]html$"
)]
current[, `:=`(
  Competition = paste(Country, "Top Flight"),
  CompetitionType = "league"
)]

# The principal cross-border club files already present in the cache.
continental <- data.table::data.table(
  Country = c("Asia", "Africa", "North/Central America", "South America", "Oceania"),
  Confederation = c("AFC", "CAF", "CONCACAF", "CONMEBOL", "OFC"),
  Directory = c("tablesa", "tablesa", "tablesc", "sacups", "tableso"),
  FilePattern = c(
    "^ascup(?:[0-9]{2}|[0-9]{4})[.]html$",
    "^afcup(?:[0-9]{2}|[0-9]{4})[.]html$",
    "^cacups(?:[0-9]{2}|[0-9]{4})[.]html$",
    "^copa(?:[0-9]{2}|[0-9]{4})[.]html$",
    "^oceacup(?:[0-9]{2}|[0-9]{4})[.]html$"
  ),
  Competition = c("AFC Club Competitions", "CAF Club Competitions",
                  "CONCACAF Club Competitions", "CONMEBOL Club Competitions",
                  "OFC Champions League"),
  CompetitionType = "continental"
)
continental <- continental[Confederation %in% requested_confeds]

configs <- unique(data.table::rbindlist(list(
  current[, .(Country, Confederation, Directory, FilePattern, Competition, CompetitionType)],
  continental
), fill = TRUE))

if (!nzchar(Sys.getenv("WORLD_AUDIT_MIN_YEAR"))) Sys.setenv(WORLD_AUDIT_MIN_YEAR = "2010")
if (!nzchar(Sys.getenv("WORLD_AUDIT_MAX_YEAR"))) Sys.setenv(WORLD_AUDIT_MAX_YEAR = "2025")
if (!nzchar(Sys.getenv("WORLD_AUDIT_RESUME"))) Sys.setenv(WORLD_AUDIT_RESUME = "1")

Sys.setenv(RSSSF_OFC_FUNCTIONS_ONLY = "1")
source(file.path(root, "scripts/EuropeanFootball/rsssf_ofc_audit.R"), encoding = "UTF-8")
Sys.unsetenv("RSSSF_OFC_FUNCTIONS_ONLY")

run_rsssf_ofc_audit(
  config_override = configs,
  audit_name = "world non-UEFA",
  output_folder = Sys.getenv("WORLD_AUDIT_OUTPUT_FOLDER", "RSSSF_World_Audit"),
  env_prefix = "WORLD_AUDIT",
  completion_beep = !identical(Sys.getenv("WORLD_AUDIT_COMPLETION_BEEP"), "0")
)
