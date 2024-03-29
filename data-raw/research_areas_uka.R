library(readxl)
library(dplyr)
library(tidyr)

uka_xlsx <- function() {
  "https://www.uka.se/vara-resultat/statistik/flikar/information-om-statistiken" |>
  rvest::read_html() |> rvest::html_elements("a") |> rvest::html_attr("href") |>
  grep(pattern = "\\.xlsx", value = TRUE)
}

research_areas_uka <- function() {

  links <- uka_xlsx()

  url <- links |> grep(pattern = "standard-2011", value = TRUE)
  url2 <- links |> grep(pattern = "nyckel-forskning", value = TRUE)

  fn <- tempfile()
  fn2 <- tempfile()
  on.exit(unlink(fn), unlink(fn2))

  download.file(destfile = fn, quiet = TRUE,
    url = paste0("https://www.uka.se", url))

  download.file(destfile = fn2, quiet = TRUE,
    url = paste0("https://www.uka.se", url2)
  )

  topics <-
    read_excel(fn, col_names = c("l1", "l2", "l3", "l4", "desc_swe", "desc_eng")) |> #, "desc_swe", "desc_eng")) |>
    # skip header
    slice(-c(1)) |>
    # skip fourth level column (blank)
    select(-l4) |>
    # remove rows where l1 ... l4 are all missing (repeating headers)
    filter(!if_all(starts_with("l"), is.na)) |> # !all(is.na(c_across(l1:l3))))  |>
    rowwise() |>
    mutate(scb_code = paste0(collapse = ",", na.omit(c_across(l1:l3)))) |>
    ungroup() |>
    #fill(l1, l2, l3, l4, .direction = "down") |>
    mutate(level = nchar(scb_code)) |>
    mutate(level = (level + 1) / 2)

  topicmap <-
    read_excel(fn2, col_names = c("old_desc", "old_code", "scb_code", "scb_swe")) |>
    slice(-c(1:8)) |>
    fill(1, 2) |>
    # remove cruft (repeating headers)
    filter(!(grepl("Standard", scb_code))) |>
    # remove rows with no description (blanks)
    filter(!is.na(scb_swe)) |>
    # remove "freely mappable" categories
    filter(!(grepl("Temaämne", scb_swe))) |>
    # convert id to integer
    mutate(scb_coden = readr::parse_vector(scb_code, readr::col_integer())) |>
    # sort by tree order
    arrange(-desc(scb_code))

  topics # |>
    #left_join(topicmap, by = "scb_code") |>
    #select(scb_id = scb_code, l1, l2, l3, level, scb_swe,
    #       desc_swe, desc_eng, old_code, old_desc) |>
    #mutate(desc_eng = gsub("[(].*?[)]$", "", desc_eng))
}

ra <- research_areas_uka()

research_areas <-
  ra |>
  select(scb_code, desc_swe, desc_eng, level) |>
  #filter(!is.na(scb_id)) |>
  #filter(is.na(desc_swe))
  rename(
    id = scb_code,
    swe = desc_swe,
    eng = desc_eng
  )

# look at some extracts
ra |> filter(l2 == 107)
research_areas |> filter(id == 107)
research_areas |> filter(level == 1) |> nrow() == 6
research_areas |> filter(level == 2) |> nrow() == 42
research_areas |> filter(level == 3) |> nrow() == 260

# five digit codes for natural sciences
research_areas |> filter(grepl("^1", as.character(id)), level == 3)

sinew::makeOxygen(research_areas)
usethis::use_data(research_areas, overwrite = TRUE)

#' @title Research Subject Areas from https://uka.se
#' @description See \url{https://www.uka.se/statistik--analys/information-om-statistiken/amneslistor-och-huvudomraden/2017-02-14-forskningsamnen.html}
#' @format A data frame with 308 rows and 4 variables:
#' \describe{
#'   \item{\code{id}}{integer subject area code}
#'   \item{\code{level}}{double a level, granularity for code (1..3)}
#'   \item{\code{swe}}{character description in Swedish}
#'   \item{\code{eng}}{character description in English}
#'}
#' @details Data from \url{https://www.uka.se/download/18.7391c377159bc0155b81ef8/1487841861615/forskningsamnen-standard-2011.xlsx}
"research_areas"

library(tibble)
library(dplyr)
library(purrr)
library(stringr)

library(magick)

pdf <- image_read_pdf("https://www.scb.se/contentassets/10054f2ef27c437884e8cde0d38b9cc4/oversattningsnyckel-forskningsamnen.pdf")
pdf_n5 <- pdf[9:19]

library(tesseract)
#tesseract_download(lang = "eng")

p1 <-
  image_ocr(pdf_n5, language = "swe") |>
  str_split("\n") |> unlist

# regexp template for valid record (4 groups of data per row)
re_record <- "((.*?)\\s+(\\d{4}))*\\s*(\\d{5})+\\s+(.*?)$"

t_n5 <-
  tibble(text = p1) |>
  # is it a valid row? does the row match the regexp template?
  mutate(is_record = str_detect(text, re_record)) |>
  # or is it an orphan (separate row but has a record above it and no numbers in it)
  mutate(is_orphan = !is_record & lag(is_record) & !str_detect(text, "\\d+")) |>
  # mark rows that have orphans
  mutate(has_orphan = lead(is_orphan)) |>
  # for those rows, add the orphan text
  mutate(text_new = if_else(has_orphan, paste(text, lead(text)), text)) |>
  # then just focus on the fixed up valid rows
  filter(str_detect(text_new, re_record)) |>
  # parse these records to capture the groups of data in each record
  pull(text_new) |> str_match(re_record) |> as_tibble() |>
  # order the capture groups
  select(4, 3, 5, 6) |>
  # fill in the blanks in the first two columns
  fill(1, 2) |>
  # use sensible field names
  select(old_code = 1, old_desc = 2, scb_code = 3, scb_desc = 4)

