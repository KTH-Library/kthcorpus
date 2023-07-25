#' Retrieve DiVA publications for KTH from the KTH DiVA portal
#'
#' This function sends a request to KTH's DiVA portal for a CSV data export
#' covering KTH publications between 2013 and 2022.
#'
#' @param orgid the DiVA organisation id, by default "177" for KTH
#' @param year_beg the beginning of the period, by default "2013"
#' @param year_end the end of the period, by default "2022"
#' @param use_cache logical flag to indicate if cached data should be used, default: TRUE
#' @return data frame with results
#' @importFrom jsonlite toJSON
#' @importFrom httr parse_url build_url
#' @importFrom curl curl_download
#' @importFrom rappdirs app_dir
#' @import readr
kth_diva_pubs_deprecated <- function(orgid = "177", year_beg = "2013", year_end = "2022", use_cache = TRUE) {
  diva_tmp <- function(file) file.path(rappdirs::app_dir("kthcorpus")$config(), file)
  tmp <- diva_tmp("kth_diva_pubs.rds")
  if (!dir.exists(dirname(tmp))) dir.create(dirname(tmp), recursive = TRUE)
  if (file.exists(tmp)) {
    return(readr::read_rds(tmp))
  }

  message("Please be patient, the export takes a few minutes to complete...")

  pubtypes <- function() {
    c(
      "bookReview", "review", "article",
      "artisticOutput", "book", "chapter",
      "manuscript", "collection", "other",
      "conferencePaper", "patent", "conferenceProceedings",
      "report", "dataset"
    )
  }

  queryparam_aq2 <- function(.pubtypes = pubtypes()) {
    list(list(
      list(dateIssued = I(list(from = year_beg, to = year_end))),
      list(organisationId = orgid, `organisationId-Xtra` = TRUE),
      list(publicationTypeCode = .pubtypes)
    )) %>% jsonlite::toJSON(auto_unbox = TRUE)
  }

  smash_url <- httr::parse_url(paste0(config$portal, "/smash/export.jsf"))

  # add any params? "language=en&searchType=RESEARCH&query=&af=[]&onlyFullText=false&sf=all"
  smash_url$query <- list(
    format = "csvall2", addFilename = "true",
    aq = I("[[]]"), aqe = I("[]"), aq2 = I(queryparam_aq2()),
    onlyFullText = "false", noOfRows = as.character(5e6L),
    sortOrder = "title_sort_asc", sortOrder2 = "title_sort_asc"
  )

  smash_file <- tempfile() # "~/repos/semanticscholar/data-raw/kth.csv"
  on.exit(unlink(smash_file))

  url <- httr::build_url(smash_url)
  message("Starting download from ", url)
  cu <- curl::curl_download(url, destfile = smash_file, quiet = TRUE)
  message("Download done.")

  ct <- readr::cols(
    .default = col_character(),
    PID = col_double(),
    Year = col_double(),
    PMID = col_double(),
    Opponents = col_logical(),
    Supervisors = col_logical(),
    Examiners = col_logical(),
    ThesisLevel = col_logical(),
    Credits = col_logical(),
    Programme = col_logical(),
    Subject = col_logical(),
    Uppsok = col_logical(),
    DefencePlace = col_logical(),
    DefenceLanguage = col_logical(),
    DefenceDate = col_logical(),
    CreatedDate = col_date(format = ""),
    PublicationDate = col_date(format = ""),
    LastUpdated = col_date(format = ""),
    NumberOfAuthors = col_double(),
    ExternalCooperation = col_logical(),
#    FridaLevel = col_double(),
    Term = col_logical(),
    Reviewed = col_logical(),
    FreeFulltext = col_logical(),
    SustainableDevelopment = col_logical()
  )

  pubs <- readr::read_csv(cu, col_types = ct)

  if (use_cache) readr::write_rds(pubs, tmp)

  return(pubs)
}


#' Retrieve DiVA authors for KTH from the KTH DiVA portal
#'
#' This function returns parsed author information from DiVA data
#'
#' @param orgid the DiVA organisation id, by default "177" for KTH
#' @param year_beg the beginning of the period, by default "2013"
#' @param year_end the end of the period, by default "2020"
#' @param use_cache logical flag to indicate if cached data should be used,
#' default: TRUE
#' @param refresh_cache logical flag to indicate if data cache should be
#' refreshed, default: FALSE
#' @return data frame with results
#' @importFrom readr write_rds read_rds
kth_diva_authors_deprecated <- function(orgid = "177", year_beg = "2013", year_end = "2022",
                             use_cache = TRUE, refresh_cache = FALSE) {
  diva_tmp <- function(file) file.path(rappdirs::app_dir("kthcorpus")$config(), file)
  tmp <- diva_tmp("kth_diva_authors.rds")
  if (!dir.exists(dirname(tmp))) dir.create(dirname(tmp), recursive = TRUE)
  if (file.exists(tmp) && !refresh_cache) {
    return(readr::read_rds(tmp))
  }

  .pubs <- kth_diva_pubs_deprecated(orgid, year_beg, year_end, use_cache)
  parsed_diva_names <- parse_diva_names(pubs = .pubs)

  if (use_cache) readr::write_rds(parsed_diva_names, tmp)

  return(parsed_diva_names)
}

#' Retrieve DiVA authors for KTH from the KTH DiVA portal
#'
#' This function returns parsed author information from DiVA data
#' @param use_cache logical flag to indicate locally cached data should be used,
#' default: TRUE
#' @param refresh_cache logical flag to indicate if local data cache should be
#' refreshed, default: FALSE
#' @return data frame with results
#' @export
kth_diva_authors <- function(use_cache = TRUE, refresh_cache = FALSE) {

  diva_tmp <- function(file)
    file.path(rappdirs::app_dir("kthcorpus")$config(), file)

  tmp <- diva_tmp("kth_kda.rds")

  if (!dir.exists(dirname(tmp))) dir.create(dirname(tmp), recursive = TRUE)
  if (file.exists(tmp) && !refresh_cache) {
    return(readr::read_rds(tmp))
  }

  res <- diva_download_s3(files = "aut.csv")

  stopifnot(res == 0 && file.exists(diva_tmp("aut.csv")))

  ct <- readr::cols(
    .default = col_character(),
    PID = col_integer(),
    Position = col_integer(),
    PMID = col_double()
  )

  LastName <- FirstName <- LocalId <- ORCID <- OrganisationId <-
    UncontrolledOrganisation <- ResearchGroup <- Role <-
    orgid <- pos <- AuthorityPid <- NULL

  data <-
    readr::read_csv(diva_tmp("aut.csv"), col_types = ct) %>%
    mutate(name = paste0(LastName, ", ", FirstName)) %>%
    rename(
      kthid = LocalId,
      orcid = ORCID,
      orgid = OrganisationId,
      autid = AuthorityPid,
      extorg = UncontrolledOrganisation,
      pos = Position,
      group = ResearchGroup,
      role = Role
    ) %>%
    select(-c(FirstName, LastName)) %>% #, DOI, ISI, ISRN, NBN, PMID, ScopusId)) %>%
    select(PID, name, kthid, orcid, orgid, extorg, pos, everything()) %>%
    mutate(uses_etal = NA, is_extorg = !is.na(extorg))

  if (use_cache) readr::write_rds(data, tmp)

  return(data)

}

#' Retrieve DiVA authors for the default org from the DiVA portal
#'
#' This function returns parsed author information from DiVA data
#' @param config settings for DiVA client, by default using diva_config()
#' @return data frame with results
#' @export
diva_authors <- function(config = diva_config()) {

  AuthorityPid <- FirstName <- LastName <- LocalId <- ORCID <- OrganisationId <-
    ResearchGroup <- Role <- UncontrolledOrganisation <- orgid <- pos <- NULL

  diva_download(
    "aut",
    org_id = config$id,
    year_beg = config$ybeg,
    year_end = config$yend,
    portal = config$portal
  ) %>%
  mutate(name = paste0(LastName, ", ", FirstName)) %>%
  rename(
    kthid = LocalId,
    orcid = ORCID,
    orgid = OrganisationId,
    autid = AuthorityPid,
    extorg = UncontrolledOrganisation,
    pos = Position,
    group = ResearchGroup,
    role = Role
  ) %>%
  select(-c(FirstName, LastName)) %>% #, DOI, ISI, ISRN, NBN, PMID, ScopusId)) %>%
  select(PID, name, kthid, orcid, orgid, extorg, pos, everything()) %>%
  mutate(uses_etal = NA, is_extorg = !is.na(extorg))
}

#' Retrieve DiVA publications for the default org from the DiVA portal
#'
#' This function sends a request to te DiVA portal from a CSV data export
#' covering publications in the default time period.
#'
#' @return data frame with results
#' @param config settings for DiVA client, by default using diva_config()
#' @export
diva_pubs <- function(config = diva_config()) {

  diva_download(
    "pub",
    org_id = config$id,
    year_beg = config$ybeg,
    year_end = config$yend,
    portal = config$portal
  )
}

#' Retrieve DiVA publications for KTH from the KTH DiVA portal
#'
#' This function sends a request to KTH's DiVA portal from a CSV data export
#' covering KTH publications between 2013 and 2022.
#'
#' @param use_cache logical flag to indicate locally cached data should be used,
#' default: TRUE
#' @param refresh_cache logical flag to indicate if local data cache should be
#' refreshed, default: FALSE
#' @return data frame with results
#' @importFrom rappdirs app_dir
#' @import readr
#' @export
kth_diva_pubs <- function(use_cache = TRUE, refresh_cache = FALSE) {

  diva_tmp <- function(file)
    file.path(rappdirs::app_dir("kthcorpus")$config(), file)

  tmp <- diva_tmp("kth_kdp.rds")

  if (!dir.exists(dirname(tmp))) dir.create(dirname(tmp), recursive = TRUE)
  if (file.exists(tmp) && !refresh_cache) {
    return(readr::read_rds(tmp))
  }

  res <- diva_download_s3(files = "pub.csv")

  stopifnot(res == 0 && file.exists(diva_tmp("pub.csv")))

  ct <- readr::cols(
    .default = col_character(),
    PID = col_double(),
    Year = col_double(),
    PMID = col_double(),
    CreatedDate = col_date(format = ""),
    PublicationDate = col_date(format = ""),
    LastUpdated = col_date(format = ""),
    NumberOfAuthors = col_double()
  )

  data <-
    readr::read_csv(diva_tmp("pub.csv"), col_types = ct)

# NB: Disabled in order to catch more multiplettes 2023-06-12
#  data <-
#    data %>%
#    filter(grepl("QC", Notes))

  if (use_cache) readr::write_rds(data, tmp)

  return(data)

}

#' Name aliases for DiVA authors in the KTH DiVA portal
#'
#' This function returns data for authors that for the same author identifier
#' have multiple names registered in composite DiVA Name strings
#' (bibliographic names for a publication).
#'
#' @param authors a tibble with authors data, default: kth_diva_authors()
#' @return data frame with results
#' @export
#' @import dplyr
kth_diva_aliases <- function(authors = kth_diva_authors()) {
  namez <- authors

  # aliases (kthids with more than one name variation)
  aliases_kthid <-
    namez %>%
    filter(!is.na(kthid) & !is.na(name)) %>%
    group_by(kthid) %>%
    mutate(n2 = n_distinct(name)) %>%
    filter(n2 > 1) %>%
    arrange(desc(n2)) %>%
    distinct(kthid, name, n2) %>%
    ungroup() %>%
    arrange(desc(n2), kthid, name) %>%
    select(kthid, name, n2)

  # aliases (orcids with more than one name variation)
  aliases_orcid <-
    namez %>%
    filter(!is.na(orcid) & !is.na(name)) %>%
    group_by(orcid) %>%
    mutate(n2 = n_distinct(name)) %>%
    filter(n2 > 1) %>%
    arrange(desc(n2)) %>%
    distinct(orcid, name, n2) %>%
    ungroup() %>%
    arrange(desc(n2), orcid, name) %>%
    select(orcid, name, n2)

  aliases_kthid %>%
    left_join(aliases_orcid) %>%
    select(kthid, orcid, n = n2, name) %>%
    collect()
}

diva_backup <- function(file) {

  ffr <- diva_tmp(file)
  fto <- diva_tmp(insert_ts(file))

  message("Backing up ", ffr, " to ", fto)
  if (file.exists(ffr))
    file.rename(ffr, fto)

  return(file.exists(fto))
}

#' Refresh locally cached data files (and backup older data)
#' @return data frame with metadata including timestamp and age (in hours)
#' @export
diva_refresh <- function() {
  refreshed <- all(
    diva_backup("kth_kda.rds"),
    diva_backup("kth_kdp.rds")
  )
  if (!refreshed)
    warning("Not all files refreshed...")

  return (refreshed)
}

diva_tmp <- function(file)
  file.path(rappdirs::app_dir("kthcorpus")$config(), file)

insert_ts <- function(file) {
  file.path(paste0(tools::file_path_sans_ext(file), "_",
                   format(Sys.time(), "%Y%m%d%H%M"), ".",
                   tools::file_ext(file))
  )
}

diva_upload_s3 <- function(path, dest = "kthb/kthcorpus", options = "") {

  util <- "mc"

  if (startsWith(Sys.getenv("OS"), "Windows"))
    util <- "mc.exe"

  stopifnot(nzchar(Sys.which(util)) || all(file.exists(path)))

  #hr_ls("kthcorpus") %>%
  # mutate(age = lubridate::as_datetime(LastModified)) %>%
  # mutate(age = lubridate::as.difftime(Sys.time() - age))

  # allow for upload of several files to the same dest bucket
  if (length(path) > 1) path <- paste0(collapse = " ", path)

  if (Sys.getenv("MC_HOST_kthb") == "" & !file.exists("~/.mc/config.json"))
    warning("Please see if envvar MC_HOST_kthb has been set, or that ~/.mc/config.json exists")

  # assemble command to invoke from shell
  # ... options can be "--newer-than 7d10h"
  cmd <- sprintf("%s cp %s %s %s", util, options, path, dest)
  res <- system(cmd, timeout = 60 * 3)

  if (res == 124)
    stop("Time out when uploading file ", path)

  if (res != 0)
    stop("Error when using minio client to upload file, error code: ", res)

  return (invisible(res))
}

diva_download_s3 <- function(files, destination, source = "kthb/kthcorpus", options = "") {

  util <- "mc"

  if (startsWith(Sys.getenv("OS"), "Windows"))
    util <- "mc.exe"

  if (missing(destination))
    destination <- file.path(rappdirs::app_dir("kthcorpus")$config())

  stopifnot(nzchar(Sys.which(util)) || all(dir.exists(destination)))

  paths <- file.path(source, files)

  if (length(files) > 1)
    paths <- paste0(collapse = " ", file.path(source, files))

  if (Sys.getenv("MC_HOST_kthb") == "" & !file.exists("~/.mc/config.json"))
    warning("Please see if envvar MC_HOST_kthb has been set, or that ~/.mc/config.json exists")

  # options can be "--newer-than 7d10h"
  cmd <- sprintf("%s cp %s %s %s", util, options, paths, destination)
  res <- system(cmd, timeout = 300)
  #res <- cmd

  if (res == 124)
    stop("Time out when downloading file(s) ", paths)

  if (res != 0)
    stop("Error when using minio client to download file, error code: ", res)

  return (invisible(res))
}

#' Metadata for cached data files
#' @return data frame with metadata including timestamp and age (in hours)
#' @export
diva_meta <- function() {

  sources <- c(
    "kth_kda.rds",
    "kth_kdp.rds"
  )

  timez <- file.mtime(diva_tmp(sources))

  age <- as.difftime(Sys.time() - timez, format = "%H:%M:%OS3")

  data.frame(source = sources, ts = timez, age = age)
}

diva_url <- function(
  orgid = diva_config()$id,
  year_beg = diva_config()$ybeg, year_end = diva_config()$yend,
  variant = c("pub", "aut"), portal = diva_config()$portal) {

  pubtypes <- function() {
    # excludes dissertations, theses and licentiate theses
    c(
      "bookReview", "review", "article",
      "artisticOutput", "book", "chapter",
      "manuscript", "collection", "other",
      "conferencePaper", "patent", "conferenceProceedings",
      "report", "dataset"
    )
  }

  queryparam_aq2 <- function(.pubtypes = pubtypes(), use_orgid = TRUE) {

    params <- list(list(
      list(dateIssued = I(list(from = year_beg, to = year_end))),
      list(organisationId = orgid, `organisationId-Xtra` = TRUE),
      list(publicationTypeCode = .pubtypes)
    ))

    # remove the organisationId slot if required
    if (!use_orgid) {
      params[[1]][[2]] <- NULL
#      params[[1]][[2]]$organisationId <- NULL
      #params[[1]][[2]]$`organisationId-Xtra` <- NULL
      #params[[1]] <- NULL
    }

    params %>% jsonlite::toJSON(auto_unbox = TRUE)
  }

  smash_url <- httr::parse_url(paste0(portal, "/smash/export.jsf"))

  # TODO: add any more params, for example...
  # "language=en&searchType=RESEARCH&query=&af=[]&onlyFullText=false&sf=all"
  smash_url$query <- switch(
    match.arg(variant),
    "aut" = list(
      format = "csv",
      addFilename = "true",
      aq = I("[[]]"),
      aqe = I("[]"),
      aq2 = I(queryparam_aq2(use_orgid = FALSE)),  # since FALSE times out (> 5 min dl)
      onlyFullText = "false",
      noOfRows = as.character(5e6L),
      sortOrder = "title_sort_asc",
      sortOrder2 = "title_sort_asc",
      csvType = "person", fl = I(paste0(
        "PID,AuthorityPid,DOI,FirstName,ISI,ISRN,LastName,LocalId,",
        "NBN,ORCID,OrganisationId,UncontrolledOrganisation,Position,PMID,",
        "ResearchGroup,Role,ScopusId"))
    ),
    "pub" = smash_url$query <- list(
      format = "csvall2",
      addFilename = "true",
      aq = I("[[]]"),
      aqe = I("[]"),
      aq2 = I(queryparam_aq2(use_orgid = FALSE)),
      onlyFullText = "false",
      noOfRows = as.character(5e6L),
      sortOrder = "title_sort_asc",
      sortOrder2 = "title_sort_asc"
    )
  )

  httr::build_url(smash_url)

}

diva_download_aut <- function(
  orgid = diva_config()$id,
  year_beg = diva_config()$ybeg, year_end = diva_config()$yend,
  sync = TRUE, diva_portal = diva_config()$portal) {

  fn <- dl <- NULL

  yb <- as.integer(year_beg)
  ye <- as.integer(year_end)

  be <- function(beg, end) {
    stopifnot(is.integer(beg) && is.integer(end) && (end >= beg))
    data.frame(
      orgid = orgid, variant = "aut",
      year_beg = beg:end, year_end = beg:end,
      portal = diva_portal
    )
    #year_beg = beg:(end -1), year_end = (beg + 1):end)
  }

  mydir <- file.path(rappdirs::app_dir("kthcorpus")$config())
  if (!dir.exists(mydir)) dir.create(mydir, recursive = TRUE)
  myfile <- function(f) file.path(mydir, f)

  # NB: the actual download command is provided through the diva_url() fcn
  all_years <-
    be(yb, ye) %>%
    purrr::pmap_chr(diva_url) %>%
    tibble(dl = .) %>%
    bind_cols(be(yb, ye)) %>%
    mutate(fn = paste0("persons_", year_beg, ".csv")) %>%
    mutate(curl = paste0(sprintf("-o %s/", mydir), fn, " '", dl, "' \\"))

  curl_ua <- ""
  #curl_ua <- paste0("-A DIVA-'", toupper(diva_config()$org), "'")
  if (nchar(curl_ua) <= 10) curl_ua <- ""

  script <- c(
    "#!/bin/bash",
    paste0("curl -Z --globoff ", curl_ua, " -L \\"),
    all_years$curl,
    "&& (awk '(NR == 1) || (FNR > 1)' persons_*.csv > aut.csv && rm persons_*.csv)"
  )


  write_lines(script, myfile("dl_aut.sh"))
  Sys.chmod(myfile("dl_aut.sh"), mode = "744")
  res <- system(sprintf("cd %s && ./dl_aut.sh", mydir), timeout = 60 * 10)

  stopifnot(file.exists(myfile("aut.csv")) && res == 0)

  if (sync) # sync to kthcorpus bucket
    diva_upload_s3(myfile("aut.csv"))

  # parse and return content
  ct <- readr::cols(
    .default = col_character()#,
    #OrganisationId = readr::col_character()
    #PID = col_integer(),
    #Position = col_integer(),
    #PMID = col_double()
  )

  #on.exit(unlink("/tmp/aut.csv"))
  readr::read_csv(myfile("aut.csv"), col_types = ct)

}

diva_download_pub <- function(
  orgid = diva_config()$id,
  year_beg = diva_config()$ybeg, year_end = diva_config()$yend,
  sync = TRUE, diva_portal = diva_config()$portal) {

  fn <- dl <- NULL

  yb <- as.integer(year_beg)
  ye <- as.integer(year_end)

  be <- function(beg, end) {
    stopifnot(is.integer(beg) && is.integer(end) && (end >= beg))
    data.frame(
      orgid = orgid, variant = "pub",
      year_beg = beg:end, year_end = beg:end,
      portal = diva_portal
    ) #year_beg = beg:(end -1), year_end = (beg + 1):end)
  }

  mydir <- file.path(rappdirs::app_dir("kthcorpus")$config())
  if (!dir.exists(mydir)) dir.create(mydir, recursive = TRUE)
  myfile <- function(f) file.path(mydir, f)

  all_years <-
    be(yb, ye) %>%
    purrr::pmap_chr(diva_url) %>%
    tibble(dl = .) %>%
    bind_cols(be(yb, ye)) %>%
    mutate(fn = paste0("pubs_", year_beg, ".csv")) %>%
    mutate(curl = paste0(sprintf("-o %s/", mydir), fn, " '", dl, "' \\"))

  curl_ua <- ""
  #curl_ua <- paste0("-A DIVA-'", toupper(diva_config()$org), "'")
  if (nchar(curl_ua) <= 10) curl_ua <- ""

  script <- c(
    "#!/bin/bash",
    paste0("curl -Z --globoff ", curl_ua, " -L \\"),
    all_years$curl,
    "&& awk '(NR == 1) || (FNR > 1)' pubs_*.csv > pub.csv && rm pubs_*.csv"
  )

  write_lines(script, myfile("dl_pubs.sh"))
  Sys.chmod(myfile("dl_pubs.sh"), mode = "744")
  res <- system(sprintf("cd %s && ./dl_pubs.sh", mydir), timeout = 60 * 10)

  stopifnot(file.exists(myfile("pub.csv")) && res == 0)

  if (sync) # sync to kthcorpus bucket
    diva_upload_s3(myfile("pub.csv"))

  # parse and return content
  # Note: on Tue 5 Jul 2022, it was discovered that col 59 (FridaLevel) is
  # not a double but contains the value "deprecated"
  ct <- readr::cols(
    .default = col_character(),
    PID = col_double(),
    Year = col_double(),
    PMID = col_double(),
    CreatedDate = col_date(format = ""),
    PublicationDate = col_date(format = ""),
    LastUpdated = col_date(format = ""),
    NumberOfAuthors = col_double()
  )

  readr::read_csv(myfile("pub.csv"), col_types = ct)

}

#' Download publication or author data from DiVA
#'
#' @param set dataset to use, either "pub" (default) or "aut"
#' @param org_id organisation to filter for, by default "177" (KTH)
#' @param year_beg first year in a range of years, default 2013
#' @param year_end last year in a range of years, default 2022
#' @param sync logical to indicate if sync should be made to s3 (default: FALSE)
#' @param portal base url for links to portal, by default diva_config()$portal
#' @export
diva_download <- function(
  set = c("pub", "aut"), org_id = diva_config()$id,
  year_beg = diva_config()$ybeg, year_end = diva_config()$yend,
  sync = FALSE, portal = diva_config()$portal) {

  utils <- c(curl = "curl", awk = "awk", mc = "mc")

  if (startsWith(Sys.getenv("OS"), "Windows"))
    utils <- paste0(utils, ".exe")

  stopifnot(has_sysreqs(utils))
  if (sync) stopifnot(has_sysreqs(utils["mc"]))

  message("Downloading ", set, " from DiVA, pls wait a couple of minutes.")
  t1 <- Sys.time()

  res <- switch(
    match.arg(set),
    "aut" =
      diva_download_aut(org_id, year_beg, year_end, sync = sync, diva_portal = portal),
    "pub" =
      diva_download_pub(org_id, year_beg, year_end, sync = sync, diva_portal = portal)
  )

  t2 <- Sys.time()
  d <- as.difftime(t2 - t1, units = "mins")

  message(sprintf("Done, took %0.2f %s", d, attr(d, "units")))
  return (res)
}

has_sysreqs <- function(utils)
  all(nzchar(Sys.which(utils)))

#' Configuration to use for DiVA client calls
#'
#' By default the configuration uses settings for KTH.
#' Settings include the full base url to the DiVA portal for the institution,
#' the organisation id in DiVA (for example "177"), the organisation abbreviation
#' (for example "kth") and ybeg (2013), yend (2022) - the range of data to include.
#' @return list with slots for settings
#' @importFrom lubridate year
#' @export
diva_config <- function() {
  list(
    portal = "https://kth.diva-portal.org",
    id = "177",
    org = "kth",
    #portal = "https://his.diva-portal.org",
    #id = "81",
    #org = "his",
    ybeg = 2010,
    yend = lubridate::year(Sys.Date())
  )
}

scrape_diva_organisations <- function(language = NULL, config = diva_config()) {

  url <- paste0(config$portal, "/smash/search.jsf?searchType=ORGANISATION")

  re_closed <- "Closed down"

  if (!is.null(language)) {

    stopifnot(language %in% c("sv", "en"))
    url <- sprintf(paste0(url, "&language=%s"), language)

    # if (language == "sv") {
    #   re_closed <- "Upphörd"
    # }
    #
    # if (language == "no") {
    #   re_closed <- "Opphørt"
    # }

  }

  page <-
    httr::GET(url) %>%
    httr::content()

  rowkey <-
    page %>%
    rvest::html_elements(css = "li[data-rowkey]") %>%
    rvest::html_attr("data-rowkey")

  orgs <-
    page %>%
    rvest::html_elements(css = "li[data-rowkey] a")

  url2 <- url3 <- url4 <- unit <- n_hits <- orgid <- is_closed <- NULL

  tibble::tibble(
    n_hits = orgs %>% rvest::html_text(),
    url = orgs %>% rvest::html_attr("href"),
    unit = orgs %>% rvest::html_attr("title")
  ) %>%
    mutate(url2 = utils::URLdecode(url)) %>%
    mutate(url3 = stringr::str_extract(url2, "aq=(.*?)$")) %>%
    mutate(url4 = stringr::str_extract(url3, "\\d+")) %>%
    select(unit,  !any_of(c("url2", "url3"))) %>%
    mutate(n = as.double(stringr::str_extract(n_hits, "\\d+"))) %>%
    mutate(url = paste0(config$portal, url)) %>%
    rename(orgid = url4) %>%
    select(unit, orgid, everything()) %>%
    mutate(is_closed = grepl(re_closed, unit)) %>%
    mutate(closed_date = stringr::str_extract(unit, "\\d{4}-\\d{2}-\\d{2}")) %>%
    mutate(unit = ifelse(is_closed, gsub("\\s+[(].*?\\d{4}-\\d{2}-\\d{2}[)]$", "", unit), unit)) %>%
    mutate(rowkey = rowkey) %>%
    mutate(level = stringr::str_count(rowkey, "_"))

}

diva_orgs <- function(config = diva_config()) {

  unit <- orgid <- rowkey <- level <- unit_sv <- is_closed <-
    closed_date <- p_rowkey <- p_orgid <- NULL

  en <-
    scrape_diva_organisations(language = "en", config)

  sv <-
    scrape_diva_organisations(language = "sv", config) %>%
    select(unit_sv = unit, orgid)

  combo <-
    left_join(en, sv, by = "orgid") %>%
    select(orgid, rowkey, level, unit_en = unit, unit_sv, n, is_closed, closed_date, url) %>%
    mutate(p_rowkey = ifelse(nchar(rowkey) > 1, gsub("_\\d+$", "", rowkey), NA_character_))

  parents <-
    combo %>% select(orgid, rowkey, p_rowkey) %>%
    left_join(combo %>% select(orgid, rowkey, p_orgid = orgid), by = c("p_rowkey" = "rowkey")) %>%
    pull(p_orgid)

  combo %>%
    mutate(p_orgid = parents) %>%
    select(orgid, p_orgid, n_diva_pubs = n, everything(), -c("rowkey", "p_rowkey")) %>%
    mutate(across(contains("orgid"), .fns = as.integer))
}

#' Get organisation data from DiVA portal
#' @export
diva_organisations <- function() {
  diva_orgs()
}

#' Refresh data trigger function
#'
#' This function can be used to automatically download data from DiVA
#' from scratch, render the report and upload it to an S3 bucket
#' @export
diva_refresh_trigger <- function() {

  # download locally and sync to S3
  aut <- diva_download("aut", sync = TRUE)
  pub <- diva_download("pub", sync = TRUE)

  # backup local files
  diva_refresh()

  # refresh the local cache (from S3)
  kth_diva_authors(refresh_cache = TRUE)
  kth_diva_pubs(refresh_cache = TRUE)

  # render the report and upload to S3
  checks_render_report()

  report <- "/tmp/checks-report.html"

  if (!file.exists(report)) {
    warning("Was not able to generate the report...")
    return (invisible(FALSE))
  }

  message("Uploading report and RDS files")
  checks_upload_report(report)
  diva_upload_s3(diva_tmp("kth_kda.rds"))
  diva_upload_s3(diva_tmp("kth_kdp.rds"))

  # delete the report locally
  #unlink(report)

  return(invisible(TRUE))
}

#' Upload DiVA ORCiD kthId pairs to object storage
#'
#' This function can be used to automatically download data from DiVA
#' from scratch, render the report and upload it to an S3 bucket
#' @export
#' @importFrom readr write_csv
diva_orcid_kthid_upload <- function() {

  # upload to bucket "kthcorpus"
  upload <- diva_orcid_kthid()
  td <- file.path(tempdir(), "diva_kthid_orcid.csv")
  readr::write_csv(upload, td)
  status <- diva_upload_s3(path = td, dest = "kthb/kthcorpus")
  message("\nStatus for upload is: ", ifelse(status == 0, "OK!", "Fail!"))
  return(invisible(status == 0))
}

diva_orcid_kthid <- function() {

  n_kthid <- n_pubs <- n_orcid <- NULL

  re_orcid <- "^([0-9]{4})+(-)+([0-9]{4})+(-)+([0-9]{4})+(-)+([0-9]{3}[0-9Xx]{1})$"
  re_kthid <- "^u1[a-z0-9]{6}$"

  aut <-
    kth_diva_authors() |> select(name, orcid, kthid, PID) |>
    filter(grepl(re_kthid, kthid), grepl(re_orcid, orcid))

  # kthids with one or more orcids
  diva_kthid_orcids <-
    aut |> group_by(kthid) |>
    summarize(
      author = paste(collapse = "|", unique(trimws(na.omit(name)))),
      n_orcid = n_distinct(orcid, na.rm = TRUE),
      n_pubs = n_distinct(PID, na.rm = TRUE)
    ) |>
    arrange(desc(n_orcid), desc(n_pubs), kthid)

  # kthid and orcid pairs (where there is a one-to-one association)
  res <-
    diva_kthid_orcids |>
    filter(n_orcid == 1) |>
    select(kthid, n_pubs) |>
    left_join(by = c("kthid"), #, "orcid", author", "n_pubs"),
#      diva_orcid_kthids |> filter(n_kthid == 1)
      aut |> distinct(kthid, orcid)
    ) |>
    select(kthid, orcid, n_pubs)

  return(res)

}
