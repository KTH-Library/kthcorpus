library(kthcorpus)
library(dplyr)

# get latest DiVA-data from minio (clear local cache)
diva_refresh()

scopus <- scopus_from_minio()
kthid_orcid_lookup <- kthid_orcid()
sids <- scopus$publications |> filter(subtype == "cp") |> pull(`dc:identifier`)

keys <-
  scopus$publications |>
  group_by(`prism:aggregationType`, subtypeDescription) |>
  count() |> arrange(desc(n)) |>
  purrr::pmap_chr(.f = function(`prism:aggregationType`, subtypeDescription, ...) frag_genre2(`prism:aggregationType`, subtypeDescription) |> names())

scopus$publications |>
  group_by(`prism:aggregationType`, subtypeDescription) |>
  count() |> arrange(desc(n)) |>
  bind_cols(key = keys)

#    `prism:aggregationType` subtypeDescription     n key
#    <chr>                   <chr>              <int> <chr>
#  1 Journal                 Article              123 article
#  2 Conference Proceeding   Conference Paper      63 conferencePaperPublished
#  3 Journal                 Review                 9 review
#  4 Book                    Book Chapter           8 chapter
#  5 Journal                 Editorial              3 editorialMaterial
#  6 Book                    Editorial              2 collection
#  7 Journal                 Note                   2 articleErratum
#  8 Book                    Book                   1 book
#  9 Conference Proceeding   Editorial              1 conferenceProceedings
# 10 Journal                 Erratum                1 articleErratum

pubs <- kth_diva_pubs()

# We can see which of those in the Scopus batch that already have been imported in DiVA
# (matches on ScopusId)
already_imported <-
  pubs |> filter(ScopusId %in% scopus$publications$eid) |> select(PID, DOI, ScopusId)

# We now match on DOIs instead DiVA could add these ScopusIds (currently missing)
pids_with_doi_and_missing_scopusid <-
  pubs |>
  filter(DOI %in% na.omit(scopus$publications$`prism:doi`)) |>
  left_join(scopus$publications, by = c("DOI" = "prism:doi")) |>
  select(PID, DOI, ScopusId = eid)

# this file could be imported to DiVA (PID and ScopusID pairs)
# this is an update in DiVA of existing records
pids_with_doi_and_missing_scopusid |> select(-DOI) |>
  write_csv("~/temp/modz/scopusid_updates_to_diva_support.csv")

intersect(pids_with_doi_and_missing_scopusid$ScopusId, already_imported$ScopusId)

# these could be imported from Scopus, since not already imported
# this is creating new records in DiVA
import <-
  scopus$publications |> filter(
    # exclude from the Scopus import, because those are already imported
    !eid %in% pids_with_doi_and_missing_scopusid$ScopusId,
    !eid %in% already_imported$ScopusId
  )

sids_cp <-
  import |>
  filter(
#    `prism:aggregationType` == "Conference Proceeding",
    `subtypeDescription` == "Conference Paper") |>
  pull(`dc:identifier`)

sids_ar <-
  import |>
  filter(
#    `prism:aggregationType` == "Conference Proceeding",
    `subtypeDescription` == "Article") |>
  pull(`dc:identifier`)

sids_other <-
  import |> filter(!`dc:identifier` %in% c(sids_cp, sids_ar)) |>
  pull(`dc:identifier`)

my_mods_ar <- scopus_mods_crawl(sids = sids_ar, scopus = scopus, ko = kthid_orcid_lookup)
my_mods_cp <- scopus_mods_crawl(sids = sids_cp, scopus = scopus, ko = kthid_orcid_lookup)
my_mods_other <- scopus_mods_crawl(sids = sids_other, scopus = scopus, ko = kthid_orcid_lookup)

#system("mkdir -p ~/temp/modz")

my_mods_other |> create_diva_modscollection() |> write_file("~/temp/modz/scopus_other.xml")
my_mods_ar |> create_diva_modscollection() |> write_file("~/temp/modz/scopus_ar.xml")
my_mods_cp |> create_diva_modscollection() |> write_file("~/temp/modz/scopus_cp.xml")

system("firefox ~/temp/modz/scopus_cp.xml")
system("firefox ~/temp/modz/scopus_ar.xml")
system("firefox ~/temp/modz/scopus_other.xml")

#---------------------------------------------
# Same procedure but for a custom scopus search for a given time interval

# Search in Scopus for a specific time period
beg <- as.Date("2023-01-01")
end <- as.Date("2023-02-28")
scopus <- scopus_search_pubs_kth(beg, end)
pubs <- kth_diva_pubs()

# investigation of combos not yet covered in "frag_genre2" (ie mappings to DiVA)
scopus$publications |>
  filter(
    `prism:aggregationType` == "Journal",
    subtypeDescription == "Short Survey"
  ) |>
  pull(`dc:identifier`) |>
  scopus_browse()

# there are the DiVA "keys" mapping to given combinations / "publication types"
keys <-
  scopus$publications |>
  group_by(`prism:aggregationType`, subtypeDescription) |>
  count() |> arrange(desc(n)) |>
  purrr::pmap_chr(.f = function(`prism:aggregationType`, subtypeDescription, ...)
    frag_genre2(`prism:aggregationType`, subtypeDescription) |> names())

scopus$publications |>
  group_by(`prism:aggregationType`, subtypeDescription) |>
  count() |> arrange(desc(n)) |>
  bind_cols(key = keys)

# We can see which of those in the Scopus batch that already have been imported in DiVA
# (because they match on ScopusId)
already_imported <-
  pubs |>
  filter(ScopusId %in% scopus$publications$eid) |>
  select(PID, DOI, ScopusId)

# We now match on DOIs instead... DiVA could add these ScopusIds (currently missing)
pids_with_doi_and_missing_scopusid <-
  pubs |>
  filter(DOI %in% na.omit(scopus$publications$`prism:doi`)) |>
  left_join(scopus$publications, by = c("DOI" = "prism:doi")) |>
  select(PID, DOI, ScopusId = eid)

# this file could be imported to DiVA (PID and ScopusID pairs)
# this is an update in DiVA of existing records
pids_with_doi_and_missing_scopusid |> select(-DOI) |>
  write_csv("~/temp/modz/scopusid_updates_to_diva_support.csv")

intersect(pids_with_doi_and_missing_scopusid$ScopusId, already_imported$ScopusId)

# these could be imported from Scopus, since not already imported
# this is creating new records in DiVA
import <-
  scopus$publications |> filter(
    # exclude from the Scopus import, because those are already imported
    !eid %in% pids_with_doi_and_missing_scopusid$ScopusId,
    !eid %in% already_imported$ScopusId
  )

sids_cp <-
  import |>
  filter(
#    `prism:aggregationType` == "Conference Proceeding",
    `subtypeDescription` == "Conference Paper") |>
  pull(`dc:identifier`)

sids_ar <-
  import |>
  filter(
#    `prism:aggregationType` == "Conference Proceeding",
    `subtypeDescription` == "Article") |>
  pull(`dc:identifier`)

sids_other <-
  import |> filter(!`dc:identifier` %in% c(sids_cp, sids_ar)) |>
  pull(`dc:identifier`)

my_mods_ar <- scopus_mods_crawl(sids = sids_ar, scopus = scopus, ko = kthid_orcid_lookup)
my_mods_cp <- scopus_mods_crawl(sids = sids_cp, scopus = scopus, ko = kthid_orcid_lookup)
my_mods_other <- scopus_mods_crawl(sids = sids_other, scopus = scopus, ko = kthid_orcid_lookup)

#system("mkdir -p ~/temp/modz")

my_mods_other |> create_diva_modscollection() |> write_file("~/temp/modz/scopus_other.xml")
my_mods_ar |> create_diva_modscollection() |> write_file("~/temp/modz/scopus_ar.xml")
my_mods_cp |> create_diva_modscollection() |> write_file("~/temp/modz/scopus_cp.xml")

system("firefox ~/temp/modz/scopus_cp.xml")
system("firefox ~/temp/modz/scopus_ar.xml")
system("firefox ~/temp/modz/scopus_other.xml")

# 25-batches per "type" (article, conference proceedings, other)
system("mkdir -p ~/temp/modz/other ~/temp/modz/ar ~/temp/modz/cp")
write_mods_chunked(my_mods_other, "~/temp/modz/other")
write_mods_chunked(my_mods_ar, "~/temp/modz/ar")
write_mods_chunked(my_mods_cp, "~/temp/modz/cp")

# these files can now be uploaded to kthb/kthcorpus/mods

# TODO: Should we exclude publications with "coverDate" set in the future?
scopus$publications |>
  filter(! eid %in% pubs$ScopusId) |>
  mutate(y = year(`prism:coverDate`)) |>
  mutate(is_earlybird = `prism:coverDate` > Sys.Date()) |>
  filter(!is_earlybird)

#### Scopus publikationer från v 35 och 36 år 2022

beg <- "2022-08-29" |> as.Date()
end <- "2022-09-11" |> as.Date()

scopus <- scopus_search_pubs_kth(beg, end)

keys <-
  scopus$publications |>
  group_by(`prism:aggregationType`, subtypeDescription) |>
  count() |> arrange(desc(n)) |>
  purrr::pmap_chr(.f = function(`prism:aggregationType`, subtypeDescription, ...) frag_genre2(`prism:aggregationType`, subtypeDescription) |> names())

scopus$publications |>
  group_by(`prism:aggregationType`, subtypeDescription) |>
  count() |> arrange(desc(n)) |>
  bind_cols(key = keys)

# We can see which of those in the Scopus batch that already have been imported in DiVA
# (because they match on ScopusId)

pubs <- kth_diva_pubs()

already_imported <-
  pubs |>
  filter(ScopusId %in% scopus$publications$eid) |>
  select(PID, DOI, ScopusId)

# We now match on DOIs instead... DiVA could add these ScopusIds (currently missing)
pids_with_doi_and_missing_scopusid <-
  pubs |>
  filter(DOI %in% na.omit(scopus$publications$`prism:doi`)) |>
  left_join(scopus$publications, by = c("DOI" = "prism:doi")) |>
  select(PID, DOI, ScopusId = eid)

# this file could be imported to DiVA (PID and ScopusID pairs)
# this is an update in DiVA of existing records
pids_with_doi_and_missing_scopusid |> select(-DOI) |>
  write_csv("~/temp/modz/v3536/scopusid_updates_to_diva_support.csv")

intersect(pids_with_doi_and_missing_scopusid$ScopusId, already_imported$ScopusId)

# these could be imported from Scopus, since not already imported
# this is creating new records in DiVA
import <-
  scopus$publications |> filter(
    # exclude from the Scopus import, because those are already imported
    !eid %in% pids_with_doi_and_missing_scopusid$ScopusId,
    !eid %in% already_imported$ScopusId
  )

sids_cp <-
  import |>
  filter(
#    `prism:aggregationType` == "Conference Proceeding",
    `subtypeDescription` == "Conference Paper") |>
  pull(`dc:identifier`)

sids_ar <-
  import |>
  filter(
#    `prism:aggregationType` == "Conference Proceeding",
    `subtypeDescription` == "Article") |>
  pull(`dc:identifier`)

sids_other <-
  import |> filter(!`dc:identifier` %in% c(sids_cp, sids_ar)) |>
  pull(`dc:identifier`)

my_mods_ar <- scopus_mods_crawl(sids = sids_ar, scopus = scopus, ko = kthid_orcid_lookup)
my_mods_cp <- scopus_mods_crawl(sids = sids_cp, scopus = scopus, ko = kthid_orcid_lookup)
my_mods_other <- scopus_mods_crawl(sids = sids_other, scopus = scopus, ko = kthid_orcid_lookup)

#system("mkdir -p ~/temp/modz")

my_mods_other |> create_diva_modscollection() |> write_file("~/temp/modz/v3536/scopus_other.xml")
my_mods_ar |> create_diva_modscollection() |> write_file("~/temp/modz/v3536/scopus_ar.xml")
my_mods_cp |> create_diva_modscollection() |> write_file("~/temp/modz/v3536/scopus_cp.xml")

system("firefox ~/temp/modz/v3536/scopus_cp.xml")
system("firefox ~/temp/modz/v3536/scopus_ar.xml")
system("firefox ~/temp/modz/v3536/scopus_other.xml")

# 25-batches per "type" (article, conference proceedings, other)
system("mkdir -p ~/temp/modz/v3536/other ~/temp/modz/v3536/ar ~/temp/modz/v3536/cp")
write_mods_chunked(my_mods_other, "~/temp/modz/v3536/other")
write_mods_chunked(my_mods_ar, "~/temp/modz/v3536/ar")
write_mods_chunked(my_mods_cp, "~/temp/modz/v3536/cp")

#######

library(tidyverse)

# these are examples of scopus identifiers associated with many authors (> 30)
eids <-
"2-s2.0-85136096142
2-s2.0-85144114333
2-s2.0-85144102310
2-s2.0-85144262179
2-s2.0-85144453181
2-s2.0-85143082466
2-s2.0-85143084563
2-s2.0-85161860997
2-s2.0-85161946420
2-s2.0-85162080709
2-s2.0-85161433187
2-s2.0-85161827135
2-s2.0-85160419394
2-s2.0-85160422771
2-s2.0-85158942427
2-s2.0-85156104827" |>
  read_lines()

# example of how to use Scopus Search API with those identifiers
# for each identifier, we get at most 100 authors ...
# ... since the Search API restricts the author listings
scopus <- eids |> scopus_search_id()
scopus$authors |> group_by(sid) |> summarize(n = n_distinct(authid))

# if we instead use the Scopus Extended Abstract API, we get more authors
# in this case, we get 2921 unique auids for the first identifier
scopus <- eids[1] |> scopus_abstract_extended()
scopus$scopus_authors  |> group_by(auid) |> count()

# NB: for this identifier we get authors with more than one set of names
# for the same auid! Possible data error? Example of such "dupes":
scopus$scopus_authors  |> group_by(auid) |> count() |>
  arrange(desc(n)) |> filter(n > 1) |>
  left_join(by = "auid",
    scopus$scopus_authors |> select(-c("id", "i"))
  ) |> distinct() |> arrange(desc(auid)) |>
  filter(auid %in% c("35227648200", "55107735600"))

# Now, we proceed to make a MODS collection for that set of scopus identifiers

# we use a helper function to generate params and create the MODs
mods_from_eid <- function(eid) {
  scopus_mods_params(
    scopus = scopus_search_id(eid),
    sid = gsub("2-s2.0-", "", eid)
  ) |>
  create_diva_mods()
}

my_mods <- eids |> map(mods_from_eid, .progress = TRUE)
my_coll <- my_mods |> create_diva_modscollection()
write_file(my_coll, file = "/tmp/atlas.xml")
system("firefox /tmp/atlas.xml")

# Now, instead use a list of scopus identifiers for conference proceedings

scopus <- scopus_from_minio()
eids <- scopus$publications |> filter(subtype == "cp") |> pull(eid)

my_mods <- eids |> map(mods_from_eid, .progress = TRUE)
my_coll <- my_mods |> create_diva_modscollection()
write_file(my_coll, file = "/tmp/cp.xml")
system("firefox /tmp/cp.xml")

# Example showing usage based on identifier lists in an Excel file

library(tidyverse)
library(readxl)

# note, this Excel file must exist at the path below
# please amend as needed...
file_gael <- "~/Downloads/SCOPUS-listor-2023.xlsx"
data_gael <- 1:9 |> map(function(x) read_excel(file_gael, sheet = x))

# sheet 3a, the fourth
missing_in_diva <-
  data_gael[[4]]

# sheet 3b, the fifth
not_kth_in_diva <-
  data_gael[[5]]

# now we proceed to create the MODS for 3a

existing <- missing_in_diva |> filter(!is.na(`PID-Candidates`))
not_imported <- missing_in_diva |> filter(is.na(`PID-Candidates`))

eids <-
  not_imported |>
  select(ScopusID) |>
  mutate(chunk = sort(1:length(ScopusID) %/% 25) + 1)

max(eids$chunk)

chunks <-
  split(eids$ScopusID, eids$chunk) |>
  map(scopus_search_id, .progress = TRUE)

# example showing how to search for identifiers in Scopus
# using chunks of 25 in order to generate too long query params in the url

scopus <- list(
  "publications" = chunks |> map("publications") |> map_df(bind_rows),
  "authors" = chunks |> map("authors") |> map_df(bind_rows),
  "affiliations" = chunks |> map("affiliations") |> map_df(bind_rows)
)

scopus$publications |> select("prism:aggregationType", "subtypeDescription") |> distinct() |>
  rename(subtype = subtypeDescription) |>
  left_join(genre_scopus_diva())

# we use a helper function to generate params and create the MODs
mods_from_eid <- function(eid) {
  scopus_mods_params(
    scopus = scopus_search_id(eid) |> suppressMessages(),
    sid = gsub("2-s2.0-", "", eid)
  ) |>
  create_diva_mods()
}

# now, we can make a helper function to run per chunk
process_chunk <- function(chunks, i, fn_prefix = "/tmp/chunk") {
  my_mods <- chunks[[i]]$publications$eid |> map(mods_from_eid, .progress = TRUE)
  my_coll <- my_mods |> create_diva_modscollection()
  fn <- glue::glue("{fn_prefix}_{sprintf('%02d', i)}.xml")
  write_file(my_coll, file = fn)
  message(fn)
}

# and we run it for all the chunks
1:length(chunks) |>
  walk(function(x) process_chunk(chunks, x, "/tmp/3a_not_imported"), .progress = TRUE)

eids <-
  existing |>
  select(ScopusID) |>
  mutate(chunk = sort(1:length(ScopusID) %/% 25) + 1)

chunks <-
  split(eids$ScopusID, eids$chunk) |>
  map(scopus_search_id, .progress = TRUE)

1:length(chunks) |>
  walk(function(x) process_chunk(chunks, x, "/tmp/3a_imported"), .progress = TRUE)


# now we proceed to 3b....
# identifiers that are not affiliated to KTH in DiVA but appears to belong to KTH in Scopus

eids <-
  not_kth_in_diva |>
  select(ScopusID) |>
  mutate(chunk = sort(1:length(ScopusID) %/% 25) + 1) |>
  rename(eid = ScopusID)

chunks <-
  split(eids$eid, eids$chunk) |>
  map(scopus_search_id, .progress = TRUE)

scopus <- list(
  "publications" = chunks |> map("publications") |> map_df(bind_rows),
  "authors" = chunks |> map("authors") |> map_df(bind_rows),
  "affiliations" = chunks |> map("affiliations") |> map_df(bind_rows)
)

# we do not have raw_org information from Scopus search API (above)
# so we need to use the Scopus Abstract Extended API, and create a helper fcn:

nordita_info <- function(eid) {
   eid |> scopus_abstract_extended() |>
   getElement("scopus_authorgroup") |>
   distinct(sid, raw_org) |>
   mutate(is_nordita = grepl("nordita", tolower(raw_org)))
}

# use it for all identifiers
ni <- eids$eid |> map_df(.progress = TRUE, nordita_info)


# once we have chunks, we want to process these, using this fcn
pc <- function(chunks, i, fn_prefix = "/tmp/chunk") {
  ids <- chunks |>  filter(chunk == i) |> pull(eid)
  my_mods <- ids |> map(mods_from_eid, .progress = TRUE)
  my_coll <- my_mods |> create_diva_modscollection()
  fn <- glue::glue("{fn_prefix}_{sprintf('%02d', i)}.xml")
  write_file(my_coll, file = fn)
  message("Wrote ", fn)
}

# these are the identifiers where the raw_org contains "nordita"
sids_nordita <- ni |>  filter(is_nordita) |> pull(sid) |> unique()

chunks_nordita <-
  tibble(eid = paste0("2-s2.0-", sids_nordita)) |>
  mutate(chunk = sort(1:length(sids_nordita) %/% 25) + 1)

# process the chunks for the "nordita" identifiers
1:max(chunks_nordita$chunk) |>
  walk(function(x) pc(chunks_nordita, x, "/tmp/3b_nordita"), .progress = TRUE)

# these are the non-nordita identifiers
sids_other <- ni |> filter(! sid %in% sids_nordita) |> pull(sid) |> unique()

chunks_other <-
  tibble(eid = paste0("2-s2.0-", sids_other)) |>
  mutate(chunk = sort(1:length(sids_other) %/% 25) + 1)

# process the chunks for the "non-nordita" identifiers
1:max(chunks_other$chunk) |>
  walk(function(x) pc(chunks_other, x, "/tmp/3b_other"), .progress = TRUE)
