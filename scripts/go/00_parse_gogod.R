# ============================================================
# Build Go games CSV from SGF files (recursive import)
#
# Output columns:
#   - Date
#   - Black
#   - White
#   - ResultCode
#   - KM
#   - Event
#   - Round
#   - Collection
#   - SourceFile
#   - RelPath
#
# Notes:
#   - Recursively scans all subfolders under sgf_root
#   - Ignores non-SGF files
#   - Extracts SGF header properties only
#   - Normalises dates to YYYY-MM-DD where possible
#   - Keeps only trusted komi values:
#       4.5, 5, 5.5, 6, 6.5, 7, 7.5
#       2.25, 2.5, 2.75, 3, 3.25, 3.5, 3.75
#     plus KM = 0 only for games before 1950-01-01
# ============================================================

library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(tibble)

options(stringsAsFactors = FALSE)

# ------------------------------------------------------------
# Paths
# ------------------------------------------------------------
sgf_root <- "C:/Users/stjuk/OneDrive/Desktop/Baduk/Go-Go-Ratings/Go/Attempt 2"
out_file <- "C:/Users/stjuk/OneDrive/Desktop/Baduk/Go-Go-Ratings/Go/Attempt 2/go_games_from_sgf.csv"

# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------

clean_name <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, "\u00A0", " ")
  x <- str_replace_all(x, "\\s+", " ")
  x <- str_trim(x)
  x[x == ""] <- NA_character_
  x
}

clean_text <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, "\u00A0", " ")
  x <- str_replace_all(x, "\\s+", " ")
  x <- str_trim(x)
  x[x == ""] <- NA_character_
  x
}

extract_prop <- function(txt, prop) {
  # Extract first property occurrence such as PB[Cho Chikun]
  # Handles simple escaped closing brackets like \]
  pattern <- paste0("(?s)", prop, "\\[((?:\\\\.|[^\\]])*)\\]")
  m <- str_match(txt, pattern)
  val <- m[, 2]
  
  if (is.na(val)) return(NA_character_)
  
  # Unescape SGF escapes
  val <- str_replace_all(val, "\\\\\\]", "]")
  val <- str_replace_all(val, "\\\\\\\\", "\\\\")
  clean_text(val)
}

normalise_sgf_date <- function(x) {
  if (is.na(x) || x == "") return(NA_character_)
  
  x <- str_trim(x)
  
  # Some files may contain multiple dates/ranges, e.g.
  # 1999-05-11,1999-05-12
  # 1999-05-11~1999-05-12
  # 1999-05-11 to 1999-05-12
  x <- str_split(x, "\\s*(,|~|to)\\s*")[[1]][1]
  x <- str_trim(x)
  
  # YYYY-MM-DD
  if (str_detect(x, "^\\d{4}-\\d{2}-\\d{2}$")) return(x)
  
  # YYYY-MM
  if (str_detect(x, "^\\d{4}-\\d{2}$")) return(paste0(x, "-01"))
  
  # YYYY
  if (str_detect(x, "^\\d{4}$")) return(paste0(x, "-01-01"))
  
  # YYYYMMDD
  if (str_detect(x, "^\\d{8}$")) {
    return(paste0(substr(x, 1, 4), "-", substr(x, 5, 6), "-", substr(x, 7, 8)))
  }
  
  # YYYY/MM/DD
  if (str_detect(x, "^\\d{4}/\\d{2}/\\d{2}$")) {
    return(str_replace_all(x, "/", "-"))
  }
  
  # Fallback: try to pull first ISO-like date from messy text
  m <- str_match(x, "(\\d{4}-\\d{2}-\\d{2})")
  if (!is.na(m[1, 2])) return(m[1, 2])
  
  NA_character_
}

normalise_result <- function(x) {
  x <- as.character(x)
  x <- str_trim(x)
  x <- str_replace_all(x, "\\s+", "")
  x <- toupper(x)
  
  keep <- str_detect(x, "^[BW]\\+")
  x[!keep] <- NA_character_
  x[x == ""] <- NA_character_
  
  x
}

normalise_komi <- function(x) {
  if (is.na(x) || x == "") return(NA_real_)
  
  x <- as.character(x)
  x <- str_replace_all(x, ",", ".")
  x <- str_trim(x)
  
  # keep only numeric content if possible
  x_num <- suppressWarnings(as.numeric(x))
  if (is.na(x_num)) return(NA_real_)
  
  x_num
}

is_trusted_komi <- function(km, date_value) {
  allowed_km <- c(
    4.5, 5.0, 5.5, 6.0, 6.5, 7.0, 7.5,
    2.25, 2.5, 2.75, 3.0, 3.25, 3.5, 3.75
  )
  
  out <- !is.na(km) & !is.na(date_value) & (km %in% allowed_km)
  out <- out | (!is.na(km) & !is.na(date_value) & date_value < as.Date("1950-01-01") & km == 0)
  
  out
}

get_collection <- function(rel_path) {
  # First folder under sgf_root
  parts <- str_split(rel_path, "[/\\\\]")[[1]]
  parts <- parts[nzchar(parts)]
  if (length(parts) >= 2L) parts[1] else NA_character_
}

# ============================================================
# Korean name detection + cleanup
# ============================================================

normalise_name_marks <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, "\\{", "(")
  x <- str_replace_all(x, "\\}", ")")
  x <- str_replace_all(x, "\u2018|\u2019", "'")
  x <- str_replace_all(x, "\u00A0", " ")
  x <- str_replace_all(x, "\\s+", " ")
  x <- str_trim(x)
  x[x == ""] <- NA_character_
  x
}

strip_suffix_bits <- function(x) {
  # Remove light metadata when detecting nationality/style
  x <- str_replace_all(x, "\\s*\\((?:m|f|am|ama|s|sr\\.?|jr\\.?|1|2)\\)\\s*$", "")
  x <- str_replace_all(x, "\\s*\\{(?:m|f|am|ama|s|sr\\.?|jr\\.?|1|2)\\}\\s*$", "")
  x <- str_replace_all(x, "\\s+Sr\\.?$", "")
  x <- str_replace_all(x, "\\s+Jr\\.?$", "")
  str_trim(x)
}

is_likely_korean_name <- function(x) {
  x <- normalise_name_marks(x)
  x <- strip_suffix_bits(x)
  
  if (is.na(x) || x == "") return(FALSE)
  
  parts <- str_split(x, "\\s+")[[1]]
  if (length(parts) < 2L) return(FALSE)
  
  surname <- parts[1]
  given   <- paste(parts[-1], collapse = " ")
  
  korean_surnames <- c(
    "Kim", "Pak", "Park", "Sin", "Shin", "Yi", "Lee", "Ryu", "Yu", "Yoo",
    "Cho", "Ch'oe", "Choi", "An", "Ahn", "Han", "Kang", "Na", "Hong",
    "Seo", "Heo", "Sim", "Im", "Jeong", "Jung", "Paek", "Baek", "Pyeon",
    "Byun", "Weon", "Won", "Mun", "Moon", "Mok", "Seol", "Pae", "Ko",
    "Kweon", "Kwon", "Hwang", "Son", "O", "Oh", "Chu", "Min", "Ham",
    "Hyeon", "Ok", "No", "Cheong", "Geum", "Keum", "Lim", "Ri"
  )
  
  if (surname %in% korean_surnames) return(TRUE)
  
  korean_markers <- c(
    "hyeon", "hyun", "seong", "seok", "seon", "seung", "seo", "seol",
    "yeong", "young", "chang", "chun", "jun", "jong", "jeong", "jung",
    "ji", "jin", "hwi", "hui", "hee", "tae", "dong", "min", "woo", "u",
    "won", "weon", "gyu", "kyu", "ho", "hoon", "hun", "sang", "chae"
  )
  
  given_norm <- tolower(given)
  hits <- sum(str_detect(given_norm, korean_markers))
  
  if (hits >= 1L && (
    str_detect(given, "-") ||
    str_detect(given, "'") ||
    length(parts) == 2L
  )) {
    return(TRUE)
  }
  
  FALSE
}

fix_korean_name <- function(x) {
  x0 <- normalise_name_marks(x)
  if (is.na(x0) || x0 == "") return(NA_character_)
  
  qualifier <- str_extract(x0, "\\s*(\\((?:m|f|am|ama|s|sr\\.?|jr\\.?|1|2)\\)|\\{(?:m|f|am|ama|s|sr\\.?|jr\\.?|1|2)\\}|(?:Sr\\.?|Jr\\.?))\\s*$")
  qualifier <- ifelse(is.na(qualifier), "", qualifier)
  
  core <- str_replace(x0, "\\s*(\\((?:m|f|am|ama|s|sr\\.?|jr\\.?|1|2)\\)|\\{(?:m|f|am|ama|s|sr\\.?|jr\\.?|1|2)\\}|(?:Sr\\.?|Jr\\.?))\\s*$", "")
  core <- str_trim(core)
  
  if (!is_likely_korean_name(core)) {
    return(x0)
  }
  
  parts <- str_split(core, "\\s+")[[1]]
  if (length(parts) < 2L) return(x0)
  
  surname <- parts[1]
  given <- paste(parts[-1], collapse = " ")
  
  surname_map <- c(
    "Sin"   = "Shin",
    "Pak"   = "Park",
    "Yi"    = "Lee",
    "Ch'oe" = "Choi",
    "Pyeon" = "Byun",
    "Paek"  = "Baek",
    "Mun"   = "Moon",
    "Yu"    = "Yoo",
    "O"     = "Oh",
    "Pae"   = "Bae",
    "Keum"  = "Geum",
    "Kweon" = "Kwon"
  )
  if (surname %in% names(surname_map)) {
    surname <- unname(surname_map[surname])
  }
  
  given <- str_replace_all(given, "'", "")
  given <- str_replace_all(given, "\\s+", " ")
  given <- str_trim(given)
  
  given <- str_replace_all(given, "(^|-|\\s)Chin", "\\1Jin")
  given <- str_replace_all(given, "(^|-|\\s)Chi",  "\\1Ji")
  given <- str_replace_all(given, "(^|-|\\s)Chae", "\\1Jae")
  given <- str_replace_all(given, "(^|-|\\s)Cho",  "\\1Jo")
  given <- str_replace_all(given, "(^|-|\\s)Pong", "\\1Bong")
  given <- str_replace_all(given, "(^|-|\\s)Weon", "\\1Won")
  given <- str_replace_all(given, "(^|-|\\s)U(?=-|$)", "\\1Woo")
  
  given <- str_replace_all(given, "Hyeon", "Hyun")
  given <- str_replace_all(given, "Heui",  "Hee")
  given <- str_replace_all(given, "Yeong", "Young")
  
  given <- str_replace_all(given, "chun$", "jun")
  given <- str_replace_all(given, "Chun$", "Jun")
  given <- str_replace_all(given, "chin$", "jin")
  given <- str_replace_all(given, "Chin$", "Jin")
  given <- str_replace_all(given, "chu$",  "joo")
  given <- str_replace_all(given, "Chu$",  "Joo")
  given <- str_replace_all(given, "hun$",  "hoon")
  given <- str_replace_all(given, "Hun$",  "Hoon")
  given <- str_replace_all(given, "su$",   "soo")
  given <- str_replace_all(given, "Su$",   "Soo")
  
  full_map <- c(
    "Sin Chin-seo"        = "Shin Jinseo",
    "Pak Cheong-hwan"     = "Park Junghwan",
    "Sin Min-chun"        = "Shin Minjun",
    "Pyeon Sang-il"       = "Byun Sangil",
    "Kang Tong-yun"       = "Kang Dongyun",
    "Pak Min-kyu"         = "Park Minkyu",
    "Yi Chi-hyeon"        = "Lee Jihyun",
    "An Seong-chun"       = "An Sungjoon",
    "Kim Cheong-hyeon"    = "Kim Junghyun",
    "Kim Cheong-hyeon Sr."= "Kim Junghyun Sr.",
    "Kim Chi-seok"        = "Kim Jiseok",
    "Yi Tong-hun"         = "Lee Donghoon",
    "Kim Myeong-hun"      = "Kim Myounghoon",
    "Seol Hyeon-chun"     = "Seol Hyunjun",
    "Han U-chin"          = "Han Woojin",
    "Song Chi-hun"        = "Song Jihoon",
    "Pak Sang-chin"       = "Park Sangjin",
    "Kang Yu-t'aek"       = "Kang Yootaek",
    "Weon Seong-chin"     = "Weon Seongjin",
    "Yi Se-tol"           = "Lee Sedol",
    "Hong Seong-chi"      = "Hong Seongji",
    "Yi Ch'ang-seok"      = "Lee Changseok",
    "Kim Chin-hwi"        = "Kim Jinhwi",
    "Yu Ch'ang-hyeok"     = "Yoo Changhyuk",
    "Han Seung-chu"       = "Han Seungjoo",
    "Yi Ch'ang-ho"        = "Lee Changho",
    "Yi Weon-yeong"       = "Lee Wonyoung",
    "Cho Han-seung"       = "Cho Hanseung",
    "Pak Chong-hun"       = "Park Jonghoon",
    "An Kuk-hyeon"        = "Ahn Kukhyun",
    "Ch'oe Chae-yeong"    = "Choi Jaeyoung",
    "Han Sang-cho"        = "Han Sangjo",
    "Pak Chin-sol"        = "Park Jinsol",
    "Pak Keon-ho"         = "Park Geunho",
    "An Cheong-ki"        = "An Jungki",
    "O Yu-chin"           = "Oh Yujin",
    "Yi Yeong-ku"         = "Lee Younggu",
    "Ch'oe Myeong-hun"    = "Choi Myeonghun",
    "Mok Chin-seok"       = "Mok Jinseok",
    "Kim Seung-chin"      = "Kim Seungjin",
    "Han Chong-chin"      = "Han Chongjin",
    "Pak Cheong-sang"     = "Park Cheongsang",
    "Ryu Min-hyeong"      = "Ryu Minhyung",
    "Yi T'ae-hyeon"       = "Lee Taehyun",
    "Mun Min-chong"       = "Moon Minjong",
    "Pak Ha-min"          = "Park Hamin",
    "Yun Chun-sang"       = "Yun Junsang",
    "Kim Ch'ae-yeong"     = "Kim Chaeyoung",
    "Pak Yeong-hun"       = "Park Yeonghun",
    "Chu Hyeong-uk"       = "Chu Hyeonguk",
    "Heo Yeong-ho"        = "Heo Youngho",
    "Yi Heui-seong"       = "Lee Huiseong",
    "Heo Yeong-rak"       = "Heo Youngrak",
    "Min Sang-yeon"       = "Min Sangyeon",
    "Pak Chi-hyeon"       = "Park Jihyun",
    "Kim Ch'ang-hun"      = "Kim Changhoon",
    "Paek Hong-seok"      = "Baek Hongseok",
    "Paek Hyeon-u"        = "Baek Hyeonwoo",
    "Yi Ho-seung"         = "Lee Hoseung",
    "Yi Weon-to"          = "Lee Wondo",
    "An Tal-hun"          = "An Dalhoon",
    "An Yeong-kil"        = "An Yeonggil",
    "Kim Seung-chae"      = "Kim Seungchae",
    "Ha Seong-pong"       = "Ha Seongbong",
    "Kim Chu-ho"          = "Kim Jooho",
    "On So-chin"          = "On Sojin",
    "Ko Keun-t'ae"        = "Ko Geuntae",
    "Song Kyu-sang"       = "Song Gyusang",
    "Han Sang-hun"        = "Han Sanghoon",
    "Chin Si-yeong"       = "Jin Siyeong",
    "Hong Ki-p'yo"        = "Hong Gipyo",
    "Kim Myeong-wan"      = "Kim Myeongwan",
    "Han Ung-kyu"         = "Han Wonggyu",
    "Yun Ch'an-heui"      = "Yun Chanhee",
    "Ha Yeong-il"         = "Ha Yeongil",
    "Pak Seung-hwa"       = "Park Seunghwa",
    "Hong Min-p'yo"       = "Hong Minpyo",
    "Keum Chi-u"          = "Geum Jiwoo",
    "Kim Se-tong"         = "Kim Setong",
    "Song T'ae-kon"       = "Song Taegon",
    "Ryu Chae-hyeong"     = "Ryu Jaehyung",
    "Pak Ch'ang-myeong"   = "Park Changmyeong",
    "Yi Cheong-u"         = "Lee Jeongwoo",
    "Yi Hyeong-chin"      = "Lee Hyeongjin",
    "Cho Hun-hyeon"       = "Cho Hunhyun",
    "Cho Tae-weon"        = "Cho Taewon",
    "Hong Se-yeong"       = "Hong Seyoung",
    "Kim Seung-chun"      = "Kim Seungjun",
    "Yi Seul-a"           = "Lee Seula",
    "An Cho-yeong"        = "An Joyeong",
    "Kim Hyeong-u"        = "Kim Hyeongwoo",
    "Yi Chae-seong"       = "Lee Jaesung",
    "Yi Chae-ung"         = "Lee Jaeung",
    "Cho Wan-kyu"         = "Cho Wangyu",
    "Kim Peom-seo"        = "Kim Beomseo",
    "Yang Chae-ho"        = "Yang Jaeho",
    "Yi Ho-peom"          = "Lee Hobeom",
    "Kim Hwan-su"         = "Kim Hwansu",
    "Kang Chi-seong"      = "Kang Jiseong",
    "Kimu Sujun"          = "Kim Sujun",
    "Yi Kang-uk"          = "Lee Kanguk",
    "Chin Tong-kyu"       = "Jin Donggyu",
    "Mun Yu-pin"          = "Moon Yubin",
    "Kim Kyeong-eun"      = "Kim Kyeongeun",
    "Yi Yeon"             = "Lee Yeon",
    "Cho Hye-yeon"        = "Cho Hyeyeon",
    "Pak Chae-keun"       = "Park Jaekeun",
    "Hong Mu-chin"        = "Hong Mujin",
    "Pak Seung-hyeon"     = "Park Seunghyun",
    "Yu Sin-hwan"         = "Yoo Sinhwan",
    "Seo Pong-su"         = "Seo Bongsoo",
    "Kim Hyeon-ch'an"     = "Kim Hyenchan",
    "Yu O-seong"          = "Yoo Oseong",
    "Yun Seong-hyeon"     = "Yun Seonghyun",
    "Cho Min-su"          = "Cho Minsu",
    "Pak Yeong-long"      = "Park Yeonglong",
    "An Hyeong-chun"      = "An Hyeongjun",
    "Yi Seong-chae"       = "Lee Seongjae",
    "Cheon Yeong-kyu"     = "Cheon Yeonggyu",
    "Kim Tong-ho"         = "Kim Dongho",
    "Yi Min-chin"         = "Lee Minjin",
    "Ch'oe Hyeon-chae"    = "Choi Hyeonchae",
    "Ch'oe Kyu-pyeong"    = "Choi Kyubyeong",
    "Yun Yeong-seon"      = "Yun Yeongseon",
    "Hong T'ae-seon"      = "Hong Taeseon",
    "Pae Chun-heui"       = "Bae Junhee",
    "Yi Ch'un-kyu"        = "Lee Chungyu",
    "Kang Ch'ang-pae"     = "Kang Changbae",
    "Mun Yeong-sam"       = "Moon Yeongsam",
    "Seo Chung-hwi"       = "Seo Joonghui",
    "Cho Sang-yeon"       = "Cho Sangyeon",
    "Kang Pyeong-kweon"   = "Kang Pyeonggwon",
    "Cheong Seo-chun"     = "Jeong Seojun",
    "O Cheong-a"          = "Oh Jeonga",
    "Kim Yeong-sam"       = "Kim Yeongsam",
    "Kim Min-ho"          = "Kim Minho",
    "Hwang Chae-yeon"     = "Hwang Jaeyeon",
    "Kim Tae-yong"        = "Kim Taeyong",
    "Kim Il-hwan"         = "Kim Ilhwan",
    "Pak Chi-yeon"        = "Park Jiyeon",
    "Pak So-yul"          = "Park Soyul",
    "Pak Seung-ch'eol"    = "Park Seungcheol",
    "Weon Chae-hun"       = "Won Jehun",
    "Ham Yeong-u"         = "Ham Yeongu",
    "Kim Ki-paek"         = "Kim Gibaek",
    "Yi Hyeon-uk"         = "Lee Hyeonuk",
    "Pak Hyeon-su"        = "Park Hyunsoo",
    "Cho Nam-ch'eol"      = "Cho Namcheol",
    "Pak Chong-uk"        = "Park Jonguk",
    "Son Keun-ki"         = "Son Geunki",
    "Kim Heui-chung"      = "Kim Huichung",
    "Paek Ch'an-heui"     = "Baek Chanhee",
    "Paek Tae-hyeon"      = "Baek Taehyun",
    "Song Hye-lyeong"     = "Song Hyeryeong",
    "Yu Pyeong-yong"      = "Yoo Pyeongyong",
    "Pak Si-yeol"         = "Park Siyeol",
    "Ch'oe Ki-hun"        = "Choi Kihun",
    "An Kwan-uk"          = "An Kwanwuk",
    "Cheong Chun-u"       = "Jeong Junwoo",
    "Cho Seung-a"         = "Cho Seungah",
    "Kim Hyeong-hwan"     = "Kim Hyeonghwan",
    "Ch'oe Weon-yong"     = "Choi Wongyong",
    "Heo Seo-hyeon"       = "Heo Seohyun",
    "Kim Sang-ch'eon"     = "Kim Sangcheon",
    "Kim Ch'an-u"         = "Kim Chanwu",
    "Sin Yun-ho"          = "Shin Yunho",
    "Yi Hyeon-chun"       = "Lee Hyeonjun",
    "Yi Min-seok"         = "Lee Minseok",
    "Cho Pyeong-t'ak"     = "Cho Pyeongtak",
    "Kim Chwa-ki"         = "Kim Jwagi",
    "Mun Il-tu"           = "Moon Ildu",
    "Kim Cheong-u"        = "Kim Jeongu",
    "Kim Ta-yeong"        = "Kim Dayoung",
    "Seo Neung-uk"        = "Seo Nungwuk",
    "Hong Man-ki"         = "Hong Manki",
    "Kang Chi-hun"        = "Kang Jihoon",
    "Song Hong-seok"      = "Song Hongseok",
    "Kim Yu-mi"           = "Kim Yumi",
    "Yu Chae-ho"          = "Yoo Jaeho",
    "Song Sang-hun"       = "Song Sanghoon",
    "Kim Min-seo"         = "Kim Minseo",
    "Kim Su-yong"         = "Kim Suyong",
    "Mun Yong-chik"       = "Moon Yongjik",
    "Yeom Tong-keon"      = "Yeom Donggeon",
    "Cheong Tu-ho"        = "Jeong Duho",
    "Heo Chin"            = "Heo Jin",
    "Kim Chu-a"           = "Kim Jooah",
    "Pak Ho-kil"          = "Park Hogil",
    "Pae Sang-yeon"       = "Bae Sangyeon",
    "Yi Sang-heon"        = "Lee Sangheon",
    "Han Chu-yeong"       = "Han Jooyoung",
    "Pak Kyeong-keun"     = "Park Kyuongkeun",
    "Seo Mu-sang"         = "Seo Musang",
    "Pak Chi-eun"         = "Park Chieun",
    "Seo Pu-kil"          = "Seo Pugil",
    "Yun Ch'un-ho"        = "Yun Junho",
    "Kim Cheong-seon"     = "Kim Jeongseon",
    "Kim Hyeon-seop"      = "Kim Hyeonseop",
    "Kim Seok-heung"      = "Kim Seokheung",
    "Pak Tae-yeong"       = "Park Taeyeong",
    "Yi Hyeong-ro"        = "Lee Hyeongro",
    "Ok Teuk-chin"        = "Ok Teukjin",
    "Ch'oe Min-sik"       = "Choi Minsik",
    "Kim Man-su"          = "Kim Mansu",
    "Cheong Hyeon-san"    = "Jeong Hyeonsan",
    "Pak Cheong-keun"     = "Park Cheonggeun",
    "Ch'oe Cheong-kwan"   = "Choi Cheonggwan",
    "Yi Ch'ae-seong"      = "Lee Jaeseong",
    "Ch'oe U-su"          = "Choi Usu",
    "Ch'oe Yun-sang"      = "Choi Yunsang",
    "Hong Seul-ki"        = "Hong Seulgi",
    "Kim Yong-su"         = "Kim Yongsu",
    "Pak Hwi-chae"        = "Park Hwichae",
    "Cheong Ch'ang-hyeon" = "Jeong Changhyeon",
    "Cho Seok-pin"        = "Cho Seokbin",
    "On Seung-hun"        = "On Seunghoon",
    "Kim Chi-eun"         = "Kim Jieun",
    "Pak T'ae-heui"       = "Park Taehee",
    "Pak Yun-seo"         = "Park Yunseo",
    "Yi Hak-yong"         = "Lee Hakyong",
    "Ch'oe Mun-yong"      = "Choi Munyong",
    "Cho Sae-pyeol"       = "Cho Saebyeol",
    "Kweon Hyeong-chin"   = "Kwon Hyeongjin"
  )
  
  original_core <- core
  
  if (original_core %in% names(full_map)) {
    out <- full_map[original_core]
    return(str_trim(paste0(out, ifelse(qualifier == "", "", paste0(" ", qualifier)))))
  }
  
  given <- str_replace_all(given, "-", "")
  out <- paste(surname, given)
  
  str_trim(paste0(out, ifelse(qualifier == "", "", paste0(" ", qualifier))))
}

parse_one_sgf <- function(path, root_dir) {
  txt <- tryCatch(
    paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
    error = function(e) NA_character_
  )
  
  if (is.na(txt)) return(tibble())
  
  rel_path <- substring(
    normalizePath(path, winslash = "/"),
    nchar(normalizePath(root_dir, winslash = "/")) + 2L
  )
  
  date_raw <- extract_prop(txt, "DT")
  black    <- extract_prop(txt, "PB")
  white    <- extract_prop(txt, "PW")
  result   <- extract_prop(txt, "RE")
  km_raw   <- extract_prop(txt, "KM")
  event    <- extract_prop(txt, "EV")
  round_no <- extract_prop(txt, "RO")
  
  tibble(
    Date       = normalise_sgf_date(date_raw),
    Black      = normalise_name_marks(black),
    White      = normalise_name_marks(white),
    ResultCode = normalise_result(result),
    KM         = normalise_komi(km_raw),
    Event      = clean_text(event),
    Round      = clean_text(round_no),
    Collection = get_collection(rel_path),
    SourceFile = basename(path),
    RelPath    = rel_path
  )
}

# ------------------------------------------------------------
# Scan SGFs recursively
# ------------------------------------------------------------
if (!dir.exists(sgf_root)) {
  stop("SGF root folder does not exist: ", sgf_root)
}

sgf_files <- list.files(
  path = sgf_root,
  pattern = "\\.sgf$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(sgf_files) == 0L) {
  stop("No SGF files found under: ", sgf_root)
}

cat("SGF files found:", length(sgf_files), "\n")

# ------------------------------------------------------------
# Parse all files
# ------------------------------------------------------------
games_raw <- map_dfr(seq_along(sgf_files), function(i) {
  if (i %% 10000L == 0L) {
    cat("Parsed", i, "files\n")
    flush.console()
  }
  
  parse_one_sgf(sgf_files[i], sgf_root)
})

# ------------------------------------------------------------
# Clean names once via unique lookup
# ------------------------------------------------------------
all_names <- unique(c(games_raw$Black, games_raw$White))
all_names <- all_names[!is.na(all_names) & all_names != ""]

cat("Unique names to process:", length(all_names), "\n")

fixed_names <- setNames(
  vapply(all_names, function(x) {
    x <- clean_name(x)
    fix_korean_name(x)
  }, character(1)),
  all_names
)

# ------------------------------------------------------------
# Clean, filter trusted KM, and dedupe
# ------------------------------------------------------------
games <- games_raw %>%
  mutate(
    Date = as.Date(Date),
    Black = clean_name(Black),
    White = clean_name(White),
    Black = if_else(!is.na(Black) & Black != "", fixed_names[Black], NA_character_),
    White = if_else(!is.na(White) & White != "", fixed_names[White], NA_character_),
    ResultCode = normalise_result(ResultCode),
    KM = as.numeric(KM),
    Event = clean_text(Event),
    Round = clean_text(Round),
    Collection = clean_text(Collection),
    SourceFile = clean_text(SourceFile),
    RelPath = clean_text(RelPath)
  ) %>%
  filter(
    !is.na(Date),
    !is.na(Black), Black != "",
    !is.na(White), White != "",
    !is.na(ResultCode), ResultCode != ""
  ) %>%
  filter(
    is_trusted_komi(KM, Date)
  ) %>%
  distinct(Date, Black, White, ResultCode, .keep_all = TRUE) %>%
  arrange(Date, Black, White, ResultCode, RelPath)

# ------------------------------------------------------------
# Save CSV
# ------------------------------------------------------------
dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
write_csv(games, out_file)

# ------------------------------------------------------------
# Summary
# ------------------------------------------------------------
cat("Rows parsed before filtering:", nrow(games_raw), "\n")
cat("Rows saved after cleaning/dedupe:", nrow(games), "\n")
cat("Date range:", as.character(min(games$Date)), "to", as.character(max(games$Date)), "\n")
cat("Saved:", out_file, "\n")

cat("\nTrusted KM distribution in saved file:\n")
print(games %>% count(KM, sort = TRUE))

cat("\nTop collections:\n")
print(games %>% count(Collection, sort = TRUE) %>% head(20))

cat("\nMissing Event count:", sum(is.na(games$Event) | games$Event == ""), "\n")
cat("Missing Round count:", sum(is.na(games$Round) | games$Round == ""), "\n")