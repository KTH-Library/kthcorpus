# R packages --------------------------------------------------------------

pkgs_deps <- c("fuzzyjoin", "RecordLinkage", "ps", "cellranger", "rvest")
installed <- installed.packages() |> tibble::as_tibble() |> getElement("Package")
idx <- which(!pkgs_deps %in% installed)
install.packages(pkgs_deps[idx])

library(kthcorpus)
library(readr)
library(dplyr)
library(fuzzyjoin)
library(RecordLinkage)
library(tibble)
library(aws.s3)

# Data sources ------------------------------------------------------------

# helper fcn to read public files from kthcorpus bucket on minio
read_kthcorpus <- function(fn) {
       "https://data.bibliometrics.lib.kth.se/kthcorpus/" |>
              paste0(fn) |> read_csv(show_col_types = FALSE)            
}

# Swecris, Formas & Vinnova
swecris <- "projects_swecris.csv" |> read_kthcorpus()
formas <- "projects_formas.csv" |> read_kthcorpus()
vinnova <- "projects_vinnova.csv" |> read_kthcorpus()

# Cordis & OpenAire
cordis <- "projects_cordis.csv" |> read_kthcorpus()
openaire <- "projects_openaire.csv" |> read_kthcorpus()

# Case
case <-
       "kthb/kthcorpus/projects_case.csv" |>
       kthcorpus:::mc_read() |>
       readr::read_csv(show_col_types = FALSE)

## Minor data wrangling steps

# Swecris.
sd <- 
       swecris |>
       rename(
              "start_date" = fundingStartDate,
              "end_date" = fundingEndDate
       ) |>       
       mutate(
              projectTitle = ifelse(is.na(projectTitleEn), projectTitleSv, projectTitleEn),
              projectTitle = gsub("\\s+"," ", iconv(tolower(projectTitle),to = "ASCII//TRANSLIT")),
              # projectTitle = stringr::str_replace_all(projectTitle,"[[:punct:]]", ""),
              agency = fundingOrganisationNameSv,
              coalese_title = coalesce(projectTitleSv,projectTitleEn),
              coalese_title = gsub("\\s+"," ", iconv(tolower(coalese_title),to = "ASCII//TRANSLIT"))
       ) |>
       mutate(dnr = gsub("(.*?)_(\\w+)$", "\\1", projectId)) |>
       # Keep only those that match the expression
       # filter(grepl("^.*(\\d{4}-\\d{4,5}).*$", dnr)) |>
       mutate(agency = recode(agency, Vetenskapsrådet = "VR")) |>
       # Select variables.
       select(any_of(c("dnr","projectId","projectTitle","coalese_title","start_date","end_date","agency"))) |>
       as_tibble()
       # select("projectTitle")

# Case
cd <- 
       case |>
       rename(
              "start_date" = beg,
              "end_date" = end
       ) |>
       mutate(across(ends_with("date"), lubridate::ymd)) |>
       mutate(dnr = gsub("[[:alpha:]]", "", `Project Number`)) |>
       mutate(dnr = gsub("^.*(\\d{4}-\\d{4,5}).*$", "\\1", dnr)) |>
       mutate(
              projectTitle = Name,
              projectTitle =  gsub("\\s+"," ", iconv(tolower(projectTitle),to = "ASCII//TRANSLIT")),
              # projectTitle = stringr::str_replace_all(projectTitle,"[[:punct:]]", ""),
              agency = `Funding Organisation`,
              # Re-code agency names.
              agency = recode(agency,
                            Energimyndigheten = "Energi",
                            Vetenskapsrådet = "VR",
                            VINNOVA = "Vinnova")
       ) |>
       mutate(dnr_org = paste0(dnr,"_",agency)) |>
       # filter(grepl("^.*(\\d{4}-\\d{4,5}).*$", dnr)) |>
       select(any_of(c(
              "efecte_id", "dnr", "projectTitle",
              "start_date", "end_date", "agency"
       ))) |>
       as_tibble()

# Case connections --------------------------------------------------------

# 1A. Case and Swecris.
#     Fuzzy matching on title using the fuzzyjoin R package.
#     For future details, the maximum distance was arbitrary set at max_dist = 3.

czs_m1 <-
       cd |> 
       filter(!is.na(projectTitle) & !duplicated(projectTitle)) |> 
       fuzzyjoin::stringdist_inner_join(
              y = sd |> filter(!duplicated(coalese_title)),
              by = c(projectTitle = "coalese_title"),
              max_dist = 3, method = "osa",
              distance_col = "osa_dist")

# Compute score: anything less than 0.8 is deemed to be a mismatch
czs_m1 <- 
       czs_m1 |>
       mutate(
              levenshtein_score = RecordLinkage::levenshteinSim(projectTitle.x,coalese_title),
              jarowinkler_score = RecordLinkage::jarowinkler(projectTitle.x,coalese_title)
       )

# |> filter(levenshtein_score >= 0.8)

# 1B. Run exact matching using ID's.
czs_m2 <- 
       cd |>
       inner_join(y = sd,by = "dnr", relationship = "many-to-many")

# 1C. Combine the results from both approaches (i.e, fuzzy with 'titles' and exact with 'ID's').
czs_m3 <- 
       bind_rows(czs_m1,czs_m2) |>
       filter(!duplicated(coalese_title)) |>
       mutate(swecris_id = ifelse(is.na(dnr.y),dnr,dnr.y))

# 1D. Connection table results.
czs_tbl <- 
       czs_m3 |> 
       dplyr::select(any_of(names(czs_m3[c("efecte_id","projectId","levenshtein_score")]))) |>
       filter(levenshtein_score != 0.00000000) |>
       add_column("from" = "case", .before = "efecte_id") |>
       add_column("to" = "swecris",.after = "efecte_id") |>
       rename(
              "from_id" = efecte_id,
              "to_id" = projectId,
              "strength" = levenshtein_score
       ) |>
       relocate("strength",.after = "to_id") |>
       arrange(desc(strength)) |>
       mutate(
              curated = NA,
              date = Sys.Date(),
              to_id = as.character(to_id)
              # matching_method = "osa"
       ) |> 
       as_tibble()

# Enrich Case ID from Swecris
# Case_ID = ifelse(is.na(Case_ID),Swecris_ID,Case_ID)) |> View()
# openxlsx::write.xlsx(file = "case-swecris_tbl.xlsx")


# 2. Case & Cordis

czc_m1 <-
       case |> 
       filter(!is.na(Name) & !duplicated(Name)) |>
       stringdist_join(
              y = cordis |> filter(!duplicated(title)),by = c(Name = "title"),
              max_dist = 3,
              method = "osa",
              mode = "inner"
       )

czc_m1 <- 
       czc_m1 |>
       mutate(levenshtein_score = RecordLinkage::levenshteinSim(Name,title)) |>
       filter(!duplicated(title), !duplicated(Name))

# Connection table results.
czc_tbl <-
       czc_m1 |>
       select(any_of(c("efecte_id", "id", "levenshtein_score"))) |>
       filter(levenshtein_score != 0.00000000) |>
       arrange(desc(levenshtein_score)) |>
       add_column("from" = "case", "to" = "cordis") |>
       rename(
              "from_id" = efecte_id,
              "to_id" = id,
              "strength" = levenshtein_score
       ) |>
       mutate(
              curated = NA,
              date = Sys.Date(),
              to_id = as.character(to_id)
       ) |>
       relocate("from",  .before = "from_id") |>
       relocate("to", .after = "from_id") |>
       as_tibble()

# 3. Case & OpenAire

colnames(openaire)[3] <- "op_id"

# Fuzzy Match.
czop <-
       case |> 
       filter(!is.na(Name)) |> 
       stringdist_join(
              y = openaire |> filter(!is.na(`Project title`)),
              by = c(Name = "Project title"),
              max_dist = 3,
              method = "osa",
              mode = "inner"
       ) |>
       mutate(levenshtein_score = RecordLinkage::levenshteinSim(Name,`Project title`)) |>
       filter(!duplicated(Name), !duplicated(`Project title`))

# Connection table results.
czop_tbl <-
       czop |>
       select(any_of(c("efecte_id","op_id","levenshtein_score"))) |>
       arrange(desc(levenshtein_score)) |>
       add_column("from" = "case", "to" = "openaire") |>
       rename(
              "from_id" = efecte_id,
              "to_id" = op_id,
              "strength" = levenshtein_score
       ) |>
       mutate(curated = NA,
              date = Sys.Date()) |>
       relocate("from",.before = "from_id") |>
       relocate("to",.after = "from_id")

# 4. Case & Vinnova

czvi <-
       case |> 
       filter(!is.na(Name)) |> 
       stringdist_join(
              y = vinnova,by = c(Name = "Ärenderubrik"),
              max_dist = 3,
              method = "osa",
              mode = "inner"
       ) |>
       mutate(levenshtein_score = RecordLinkage::levenshteinSim(Name,Ärenderubrik)) |>
       filter(!duplicated(Name), !duplicated(Ärenderubrik))

# Connection table results.
czvi_tbl <-
       czvi |>
       select(any_of(c("efecte_id","Diarienummer","levenshtein_score"))) |>
       arrange(desc(levenshtein_score)) |>
       rename(
              "from_id" = efecte_id,
              "to_id" = Diarienummer,
              "strength" = levenshtein_score
       ) |>
       mutate(
              curated = NA,
              date = Sys.Date()
       ) |>
       add_column("from" = "case", "to" = "vinnova") |>
       relocate("from",.before = "from_id") |>
       relocate("to",.after = "from_id")


# 5. Case & Formas

czfo <-
       case |> 
       filter(!is.na(Name)) |> 
       stringdist_join(
              y = formas,
              by = c(Name = "ärenderubrik"),
              max_dist = 3,
              method = "osa",
              mode = "inner"
       ) |>
       mutate(levenshtein_score = RecordLinkage::levenshteinSim(Name,ärenderubrik)) |>
       filter(!duplicated(Name), !duplicated(ärenderubrik))

# Connection table results.

czfo_tbl <-
       czfo |>
       select(any_of(c("efecte_id","diarienummer","levenshtein_score"))) |>
       arrange(desc(levenshtein_score)) |>
       rename(
              "from_id" = efecte_id,
              "to_id" = diarienummer,
              "strength" = levenshtein_score
       ) |>
       mutate(
              curated = NA,
              date = Sys.Date()
       ) |>
       add_column("from" = "case", "to" = "formas") |>
       relocate("from",.before = "from_id") |>
       relocate("to",.after = "from_id")


# Swecris connections -----------------------------------------------------

# 6. Swecris & Vinnova

# Note. This approach yield better results than using probabilistic matching via fastlink.

# Match on ID.
swvin <- 
       sd |>
       inner_join(vinnova, by = c(dnr = "Diarienummer"), keep = T)

# Connection table result.
swvin_tbl <-
       swvin |>
       select("projectId","Diarienummer") |>
       rename(
              "from_id" = projectId,
              "to_id" = Diarienummer
       ) |>
       mutate(curated = NA,
              date = Sys.Date()) |>
       add_column("from" = "swecris", "to" = "vinnova", "strength" = 1) |>
       relocate("from", .before = "from_id") |>
       relocate("to", .after = "from_id") |>
       relocate("strength", .after = "to_id")

# 7. Swecris & Formas

# Match on ID
swfor <- 
       sd |>
       inner_join(formas, by = c(dnr = "diarienummer"),keep = TRUE)

# Connection table results.
swfor_tbl <-
       swfor |>
       select("projectId","diarienummer") |>
       rename(
              "from_id" = projectId,
              "to_id" = diarienummer
       ) |>
       mutate(
              curated = NA,
              date = Sys.Date()
       ) |>
       add_column("from" = "swecris", "to" = "formas", "strength" = 1) |>
       relocate("from", .before = "from_id") |>
       relocate("to", .after = "from_id") |>
       relocate("strength", .after = "to_id")

# 8. Swecris & Cordis

swcord <- 
       sd |>
       stringdist_inner_join(cordis, 
              by = c(projectTitle = "title"),
              max_dist = 3,
              method = "osa"
       ) |>
       mutate(levenshtein_score = RecordLinkage::levenshteinSim(projectTitle,title))

swcord_tbl <-
       swcord |>
       dplyr::select("projectId","id","levenshtein_score") |>
       rename(
              "from_id" = projectId,
              "to_id" = id,
              "strength" = levenshtein_score
       ) |>
       mutate(
              curated = NA,
              date = Sys.Date(),
              from_id = as.character(from_id),
              to_id = as.character(to_id)
       ) |>
       add_column("from" = "swecris", "to" = "cordis") |>
       relocate("from", .before = "from_id") |>
       relocate("to", .after = "from_id") |>
       relocate("strength", .after = "to_id")

# 9. Swecris & Openaire

# Fuzzy match on titles.
swop <-
       sd |> filter(!is.na(projectTitle)) |>
       stringdist_inner_join(
              openaire |> 
              filter(!is.na(`Project title`)) |> 
              mutate(clean_title = gsub("\\s+"," ", iconv(tolower(`Project title`),to = "ASCII//TRANSLIT"))),
              by = c(projectTitle = "clean_title"),
              method = "osa",
              max_dist = 4
       )

swop <- swop |> mutate(levenshtein_score = RecordLinkage::levenshteinSim(projectTitle,clean_title))

# Table results.
swop_tbl <- 
       swop |> 
       select(any_of(c("projectId", "op_id", "levenshtein_score"))) |>
       rename(
              "from_id" = projectId,
              "to_id" = op_id,
              "strength" = levenshtein_score
       ) |>
       mutate(
              curated = NA,
              date = Sys.Date(),
              from_id = as.character(from_id),
              to_id = as.character(to_id)
       ) |>
       add_column("from" = "swecris", "to" = "openaire") |>
       relocate("from", .before = "from_id") |>
       relocate("to", .after = "from_id") |>
       relocate("strength", .after = "to_id")


# Cordis connections ------------------------------------------------------

# 10. Cordis & Openaire

coa_fm <- 
       cordis |>
       stringdist_inner_join(
              openaire |> filter(!is.na(`Project title`)), 
              by = c(title = "Project title"),
              method = "osa"
       ) |>
       mutate(levenshtein_score = RecordLinkage::levenshteinSim(title,`Project title`))

# Table results.
cop_tbl <-
       coa_fm |>
       select(any_of(c("id","op_id","levenshtein_score"))) |>
       rename(
              "from_id" = id,
              "to_id" = op_id,
              "strength" = levenshtein_score
       ) |>
       mutate(
              curated = NA,
              date = Sys.Date(),
              from_id = as.character(from_id),
              to_id = as.character(to_id)
       ) |>
       add_column("from" = "cordis", "to" = "openaire") |>
       relocate("from", .before = "from_id") |>
       relocate("to", .after = "from_id") |>
       relocate("strength", .after = "to_id")


# Master connection table -------------------------------------------------

master_tbl <- 
       dplyr::bind_rows(list(
              czs_tbl,czop_tbl,czvi_tbl,
              czfo_tbl,swvin_tbl,swfor_tbl,
              swcord_tbl,swop_tbl,cop_tbl
       ))

# Export to Minio ---------------------------------------------------------

message("Uploading results to minio")
temp_file <- tempfile(fileext = ".csv", tmpdir = tempdir())
write_csv(x = master_tbl, file = temp_file)
minioclient::mc_cp(temp_file, "kthb/kthcorpus/project_links.csv", verbose = TRUE)
unlink(temp_file)

# Did it work?
connection_tbl <-
  kthcorpus:::mc_read("kthb/kthcorpus/project_links.csv") |>
  readr::read_csv(show_col_types = FALSE)

message("Is uploaded file identical to local file? ",
  all.equal(as.data.frame(connection_tbl), as.data.frame(master_tbl)))
