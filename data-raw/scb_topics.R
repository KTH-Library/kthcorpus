library(openxlsx)
library(dplyr)
library(tidyr)

xlsx <- "https://www.uka.se/download/18.36bb9e318e560741e37d/1734599703430/Standard%20f%C3%B6r%20svensk%20indelning%20av%20forsknings%C3%A4mnen%202025.xlsx"

tmp <- read.xlsx(xlsx)

names(tmp) <- c("Level1", "Level2", "Level3", "Swedish", "English")

tmp <- tmp |>
  mutate(
    Level1 = as.integer(Level1),
    Level2 = as.integer(Level2),
    Level3 = as.integer(Level3),
    id = coalesce(Level1, Level2, Level3),
    level = as.integer(case_when(id < 10 ~ 1,
                                 id < 1000 ~ 2,
                                 id < 100000 ~ 3,
                                 .default = NA)),
    Swedish = trimws(Swedish),
    English = trimws(English),
    extra = is.na(id))

subjects <- tmp |>
  filter(!is.na(id)) |>
  select(id, Swedish, English, level)

comments <- tmp |>
  fill(id, .direction = "down") |>
  filter(extra,
         !grepl("Konst - vetenskaplig grund", Swedish),
         !grepl("Konst - konstnärlig grund", Swedish)) |>
  group_by(id) |>
  summarise(Swe_comment = paste(Swedish, collapse = "\n"),
            Eng_comment = paste(English, collapse = "\n"))

scb_topics <- subjects |>
  left_join(comments, by = "id")

### Sanity check
scb_topics |> filter(level == 1) |> nrow() # 6
scb_topics |> filter(level == 2) |> nrow() # 42
scb_topics |> filter(level == 3) |> nrow() # 297

scb_topics |> filter(level == 1)

scb_topics |> filter(id == 40105)

usethis::use_data(scb_topics, overwrite = TRUE)
