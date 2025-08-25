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
  purrr::pmap_chr(.f =  function(`prism:aggregationType`, subtypeDescription, ...)
    frag_genre2(`prism:aggregationType`, subtypeDescription) |> names())


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

# We now match on DOIs instead, DiVA could add these ScopusIds (currently missing)
pids_with_doi_and_missing_scopusid <-
  pubs |>
  filter(DOI %in% na.omit(scopus$publications$`prism:doi`)) |>
  left_join(scopus$publications, by = c("DOI" = "prism:doi")) |>
  select(PID, DOI, ScopusId = eid) |>
  anti_join(already_imported, by = c("PID", "DOI", "ScopusId"))

# this file could be imported to DiVA (PID and ScopusID pairs)
# this is an update in DiVA of existing records
pids_with_doi_and_missing_scopusid |> select(-DOI) |>
  write_csv("~/temp/modz/scopusid_updates_to_diva_support.csv")

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
end <- as.Date("2023-05-31")
scopus <- scopus_search_pubs_kth(beg, end)
diva_refresh()
pubs <- kth_diva_pubs()

# there are the DiVA "keys" mapping to given combinations / "publication types"
# investigation of combos not yet covered in "frag_genre2" (ie mappings to DiVA)

scopus$publications |>
  group_by(`prism:aggregationType`, subtypeDescription) |>
  count() |> arrange(desc(n)) |>
  left_join(genre_scopus_diva() |> rename(subtypeDescription = subtype))

scopus$publications |>
  filter(
    `prism:aggregationType` == "Trade Journal",
    subtypeDescription == "Review"
  ) |>
  pull(`dc:identifier`) |>
  scopus_browse()

sids <- scopus$publications |> filter(subtype == "cp") |> pull("dc:identifier")
eids <- scopus$publications |> filter(subtype == "cp") |> pull("eid")
dois <- scopus$publications |> filter(subtype == "cp") |> pull("prism:doi")
# keys <-
#   scopus$publications |>
#   group_by(`prism:aggregationType`, subtypeDescription) |>
#   count() |> arrange(desc(n)) |>
#   purrr::pmap_chr(.f = function(`prism:aggregationType`, subtypeDescription, ...)
#     frag_genre2(`prism:aggregationType`, subtypeDescription) |> names())
#
# scopus$publications |>
#   group_by(`prism:aggregationType`, subtypeDescription) |>
#   count() |> arrange(desc(n)) |>
#   bind_cols(key = keys)

# We can see which of those in the Scopus batch that already have been imported in DiVA
# (because they match on ScopusId)
already_imported <-
  pubs |>
  filter(ScopusId %in% eids) |>
  select(PID, DOI, ScopusId)

# We now match on DOIs instead... DiVA support could add these ScopusIds (currently missing)
pids_with_doi_and_missing_scopusid <-
  pubs |>
  filter(DOI %in% na.omit(dois)) |>
  left_join(scopus$publications |> filter(eid %in% eids), by = c("DOI" = "prism:doi")) |>
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
    eid %in% eids,
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

# sids_ar <-
#   import |>
#   filter(
# #    `prism:aggregationType` == "Conference Proceeding",
#     `subtypeDescription` == "Article") |>
#   pull(`dc:identifier`)
#
# sids_other <-
#   import |> filter(!`dc:identifier` %in% c(sids_cp, sids_ar)) |>
#   pull(`dc:identifier`)

#my_mods_ar <- scopus_mods_crawl(sids = sids_ar, scopus = scopus, ko = kthid_orcid_lookup)

#my_mods_markus <- scopus_mods_crawl(sids = "SCOPUS_ID:85159787071", scopus = scopus, ko = kthid_orcid_lookup)
my_mods_cp <- scopus_mods_crawl(sids = sids_cp, scopus = scopus, ko = kthid_orcid_lookup)
# no confinfo found for these identifiers:
 # SCOPUS_ID:85153507418
 # SCOPUS_ID:85152489651
 # SCOPUS_ID:85151018475
 # SCOPUS_ID:85150529759
 # SCOPUS_ID:85148497118
 # SCOPUS_ID:85146304521
#my_mods_other <- scopus_mods_crawl(sids = sids_other, scopus = scopus, ko = kthid_orcid_lookup)

#system("mkdir -p ~/temp/modz")

#my_mods_other |> create_diva_modscollection() |> write_file("~/temp/modz/scopus_other.xml")
#my_mods_ar |> create_diva_modscollection() |> write_file("~/temp/modz/scopus_ar.xml")
my_mods_cp |> create_diva_modscollection() |> write_file("~/temp/modz/scopus_cp.xml")

system("firefox ~/temp/modz/scopus_cp.xml")
#system("firefox ~/temp/modz/scopus_ar.xml")
#system("firefox ~/temp/modz/scopus_other.xml")

# 25-batches per "type" (article, conference proceedings, other)
system("mkdir -p ~/temp/modz/other ~/temp/modz/ar ~/temp/modz/cp")
#write_mods_chunked(my_mods_other, "~/temp/modz/other")
#write_mods_chunked(my_mods_ar, "~/temp/modz/ar")
write_mods_chunked(my_mods_cp, "~/temp/modz/cp")

# these files can now be uploaded to kthb/kthcorpus/mods

# TODO: Should we exclude publications with "coverDate" set in the future?
scopus$publications |>
  filter(! eid %in% pubs$ScopusId) |>
  mutate(y = year(`prism:coverDate`)) |>
  mutate(is_earlybird = `prism:coverDate` > Sys.Date()) |>
  filter(!is_earlybird)

# we do not want publications which have not yet been properly filled with
# information about volume, issue, pages... what is a good criteria to catch these?

# wos is better at signalling when a publication is "ready" and not "early bird" with
# respect to metadata...




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

#

library(tidyverse)

# these are examples of scopus identifiers associated with many authors (> 30)
eids <-
"2-s2.0-85148497118
2-s2.0-85146304521
2-s2.0-85152489651
2-s2.0-85149933573
2-s2.0-85149912365
2-s2.0-85149505088
2-s2.0-85149564601" |>
  read_lines()

scopus_search_id(eids)

.Last.value -> a

# which "combos" are these?
a$publications |> group_by(`prism:aggregationType`, subtypeDescription) |> count() |>
  ungroup() |>
  rename(subtype = subtypeDescription) |>
  left_join(genre_scopus_diva())

b <- eids |>  map(.progress = TRUE, possibly(scopus_abstract_extended))


# Number of authors?

eid <- "2-s2.0-85149045814"
d <- eid |> scopus_abstract_extended()

d$scopus_authors


params <- scopus_mods_params(
    scopus = scopus_search_id(eid) |> suppressMessages(),
    sid = gsub("2-s2.0-", "", eid)
  )

params |> create_diva_mods()




################


# TODO: investigate "quirks" related to some eids

library(tidyverse)

eids <-
"2-s2.0-85136240497
2-s2.0-85136194952
2-s2.0-85161212339
2-s2.0-85171958264
2-s2.0-85148330382
2-s2.0-85148093031
2-s2.0-85148107701
2-s2.0-85148091482
2-s2.0-85148079388
2-s2.0-85148053986
2-s2.0-85150666513
2-s2.0-85162061434
2-s2.0-85159755540
2-s2.0-85160380925
2-s2.0-85159729404
2-s2.0-85157979216
2-s2.0-85150774941
2-s2.0-85150765588
2-s2.0-85150785019
2-s2.0-85150969579
2-s2.0-85151304605
2-s2.0-85151322199
2-s2.0-85151345466
2-s2.0-85152401150
2-s2.0-85112485362
2-s2.0-85152833867
2-s2.0-85168411217
2-s2.0-85140274775
2-s2.0-85147902229
2-s2.0-85152920047
2-s2.0-85153036087
2-s2.0-85135742464
2-s2.0-85147226480
2-s2.0-85167742690
2-s2.0-85168418194
2-s2.0-85168424346
2-s2.0-85166469138
2-s2.0-85166466634
2-s2.0-85166474762
2-s2.0-85152956226
2-s2.0-85153347486
2-s2.0-85154609178
2-s2.0-85152047170
2-s2.0-85151715855
2-s2.0-85164719832
2-s2.0-85173587733
2-s2.0-85173587733
" |> read_lines()

# we use a helper function to generate params and create the MODs
mods_from_eid <- function(eid) {
  scopus_mods_params(
    scopus = scopus_search_id(eid),
    sid = gsub("2-s2.0-", "", eid)
  ) |>
  create_diva_mods()
}

aw_mods_from_eid <- possibly(mods_from_eid, otherwise = NA_character_)

# TODO: can some records / eids actually be placeholders?
# is it NLM?

my_mods <-
  eids |> map(aw_mods_from_eid, .progress = TRUE)

my_mods <- my_mods[(my_mods |> map(is.character) |> unlist())]

my_coll <- my_mods |> create_diva_modscollection()
write_file(my_coll, file = "/tmp/quirks.xml")
system("firefox /tmp/quirks.xml")

# 2-s2.0-85148093031
# 2-s2.0-85148107701
# 2-s2.0-85148091482
# 2-s2.0-85148079388
# 2-s2.0-85148053986

pubs <- kth_diva_pubs()

pubs |> filter(ScopusId %in% eids[6:10]) |>
  mutate(eid = eids[6:10]) |> select("PID", eid, DOI) |>
  mutate(link_pid = linkify(PID, target = "PID")) |>
  mutate(link_doi = linkify(DOI, target = "DOI")) |>
  DT::datatable(escape = FALSE)

# TODO: NLM publications might need special treatment...
# TODO: Exclude any pubs in KuraTHor checks that match this criteria
# TODO: I samband med MODS-genererandet ska inte "QC 20240117" med i MODS-filerna

pubs |> filter(grepl("Imported from Scopus\\. VERIFY\\.", Notes)) |> select(PID, Notes)

########## SCOPUS-listor-*.xlsx

# The data in SCOPUS-listor needs to be stacked across time
# A format similar to what swepub use (see below) could be used

library(stringr)
library(dplyr)

kdp <- kth_diva_pubs()

pids <-
"1356052
1356054
1356057" |>
  strsplit(split = "\n") |>
  unlist() |>
  as.double()


kdp |> group_by(Title) |> summarise(n_pids = n_distinct(PID)) |> arrange(desc(n_pids))

swepub_format <- function(
  config = diva_config(),
  year_beg = diva_config()$ybeg,
  year_end = diva_config()$yend
  ) {

  output_type <- mods_url <- repository_url <- publication_year <-
    value <- flag_type <- record_id <- NULL

  org <- config$org


  url <- paste0(
      "https://bibliometri.swepub.kb.se/api/v1/process/",
      sprintf("%s/export?from=%s&to=%s", org, year_beg, year_end),
      "&enrichment_flags=DOI_enriched,ISSN_enriched,ORCID_enriched",
      "&validation_flags=DOI_invalid,ISBN_invalid,ISSN_invalid,ORCID_invalid",
      "&audit_flags=creator_count_check_invalid"
#      "&audit_flags=UKA_comprehensive_check_invalid,creator_count_check_invalid"
    )


  tsv <-
    url |>
    httr::GET(config = httr::add_headers("Accept" = "text/tab-separated-values")) |>
    content(type = "text", encoding = "UTF-8")

  colz <-
    readr::read_lines(
      "record_id
      source
      publication_year
      publication_type
      output_type
      flag_class
      flag_type
      flag_code
      validation_rule
      value
      old_value
      new_value
      path
      mods_url
      repository_url\n") |>
    sapply(trimws) |>
    unname()

  tsv <- readr::read_tsv(
      tsv,
      skip = 1,
      show_col_types = FALSE,
      col_names = colz,
      locale = readr::locale(encoding = "UTF-8")
    )

  return(tsv)
}

fmt <- swepub_format()

fmt |>
  mutate(output_type = linkify(gsub("term/swepub", "term/swepub/output", output_type))) |>
    mutate(swepub_url = linkify(swepub_url(record_id), text = record_id)) |>
  head(5) |>
  mutate(PID = pid_from_urn(repository_url))




###############

library(tidyverse)

# these are examples of scopus identifiers associated with many authors (> 30)
eids <-
  "~/Downloads/ScopusId_missing_in_DiVA_2024-06-10.txt" |>
  read_lines()

ko <- kthid_orcid()

# Now, we proceed to make a MODS collection for that set of scopus identifiers

# we use a helper function to generate params and create the MODs
mods_from_eid <- function(eid) {
  scopus_mods_params(
    scopus = scopus_search_id(eid),
    sid = gsub("2-s2.0-", "", eid),
    kthid_orcid_lookup = ko
  ) |>
  create_diva_mods()
}

possibly_mods <-
  purrr::possibly(\(x) mods_from_eid(x), otherwise = NULL)

my_mods <- eids |> map(possibly_mods, .progress = TRUE)
my_fails <- map(my_mods, is.null) |> as_vector() |> which()
my_problematic_eids <- eids[my_fails]

my_problematic_eids |> paste0(collapse = "\n") |> cat()

my_coll <-
  my_mods[-c(my_problematic_eids)] |>
  create_diva_modscollection()

write_file(my_coll, file = "/tmp/aw_june_10.xml")

dir.create("/tmp/aw")

my_mods[-c(my_problematic_eids)] |>
  write_mods_chunked(, outdir = "/tmp/aw")

system("firefox /tmp/aw_june_10.xml")

###############

# Scopus files from Gaël provided by Anders W on June 5, 2025

library(tidyverse)

eids <- 
"data-raw/utsökning_missing_scopus_2025.txt" |> read_lines() |> 
  gsub(pattern = ".*?(2-s2.0-\\d{1,}).*$", replacement = "\\1")

ko <- kthid_orcid()

# Now, we proceed to make a MODS collection for that set of scopus identifiers

# we use a helper function to generate params and create the MODs
mods_from_eid <- function(eid) {
  scopus_mods_params(
    scopus = scopus_search_id(eid),
    sid = gsub("2-s2.0-", "", eid),
    kthid_orcid_lookup = ko
  ) |>
  create_diva_mods()
}

possibly_mods <-
  purrr::possibly(\(x) mods_from_eid(x), otherwise = NULL)

my_mods <- eids |> map(possibly_mods, .progress = TRUE)
my_fails <- map(my_mods, is.null) |> as_vector() |> which()
my_problematic_eids <- eids[my_fails]

my_problematic_eids |> paste0(collapse = "\n") |> cat()

my_coll <-
  my_mods |> 
  purrr::compact()

my_coll |> 
  create_diva_modscollection()

# exclude one entry which complains about C++ exception: xmlParseEntityRef: no name

# topic contains "Growth & remodeling"!
my_coll[c(333)] |> create_diva_modscollection()
my_coll[[c(333)]] |> writeLines("data-raw/2-s2.0-85184935455.xml")
system("xmllint data-raw/2-s2.0-85184935455.xml")

my_coll2 <- my_coll[-c(333)]

# topic contains "M&A strategy"!
my_coll2[[c(458)]] |> writeLines("data-raw/2-s2.0-85162999985.xml")
system("xmllint data-raw/2-s2.0-85162999985.xml")

my_coll3 <- my_coll2[-c(458)]


my_coll_file <- 
  my_coll3 |> 
  create_diva_modscollection()

write_file(my_coll_file, file = "/tmp/aw_june_5.xml")

dir.create("/tmp/aw")

my_coll3 |>
  write_mods_chunked(, outdir = "/tmp/aw")

system("firefox /tmp/aw_june_5.xml")
