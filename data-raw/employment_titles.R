library(readxl)
library(dplyr)

# ss_employment_title <-
#   # Downloaded manually into TjbLista.xls from https://www.h6.scb.se/anstallningsbenamning/
#   readxl::read_xls("data-raw/TjbLista.xls") %>%
#   rename(
#     id = Kod,
#     desc_swe = `Benämning`,
#     cat_desc = `Anställningskategori`,
#     is_uf_ta = `Personalkategori (UF/TA)`
#     ) %>%
#   mutate(across(.fns = function(x) ifelse(nchar(x) < 2, NA_character_, x)))

# 49 changes and 608 new titles (such as "Agil utvecklingsledare" for example)
#daff::diff_data(ss_employment_title, ss_employment_title2) |> 
#  daff::render_diff()

ss_employment_title <- 
  # Downloaded manually into AnstBenLista.xlsx from https://anstallningsbenamningar.scb.se/
  readxl::read_xlsx("data-raw/AnstBenLista.xlsx") |>
  rename(
    id = Kod,
    desc_swe = `Benämning`,
    cat_desc = `Anställningskategori`,
    is_uf_ta = `Personalkategori (UF/TA)`
    ) |>
  mutate(across(.cols = everything(), .fns = function(x) ifelse(nchar(x) < 2, NA_character_, x)))

sinew::makeOxygen(ss_employment_title)

usethis::use_data(ss_employment_title, overwrite = TRUE)
  
