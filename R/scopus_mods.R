#' @importFrom readr read_csv
read_from_minio <- function(fn) {
  minio_get(fn, bucket = "kthcorpus") |>
    rawToChar() |> paste0("\n") |>
    readr::read_csv(show_col_types = F, na = "NA")
}

#' Read scopus data from object storage
#' @export
scopus_from_minio <- function() {
  list(
    publications = read_from_minio("scopus-publications.csv"),
    affiliations = read_from_minio("scopus-affiliations.csv"),
    authors = read_from_minio("scopus-authors.csv")
  )
}

#' Read ORCiD kthid pairs from object storage
#' @export
kthid_orcid <- function() {
  "ug_kthid_orcid.csv" |> read_from_minio() |>
  select("kthid" = 1, "orcid" = 2) |>
  bind_rows(
    "diva_kthid_orcid.csv" |> read_from_minio()
  ) |> dplyr::distinct(kthid, orcid)
}

scopus_extent_from_pagerange <- function(x) {
  x |> strsplit(split = "\\D+") |> unlist() |>
    as.integer() |> sort() |> as.character() %>%
    list(extent_beg = .[1], extent_end = .[2])
}

#' Creates MODS for a given Scopus identifier
#' @param scopus the result from scopus_search_pubs_kth()
#' @param sid a scopus identifier (required for getting extended abstract reference data)
#' @param kthid_orcid_lookup a lookuptable of orcids with a known kthid associated from kthid_orcid() fcn
#' @return an object with parameters which can be used to generate MODS with the
#' create_diva_mods() function
#' @export
scopus_mods_params <- function(scopus, sid, kthid_orcid_lookup = kthid_orcid()) {

  `dc:identifier` <- `dc:description` <- ce_surname <-
    ce_given_name <- raw_org <- afid <-
    eng_code <- swe_code <- value <- NULL

  p <- scopus$publications %>% filter(grepl(sid, `dc:identifier`))
  aff <- scopus$affiliations %>% filter(sid == p$`dc:identifier`)
  aut <- scopus$authors %>% filter(sid == p$`dc:identifier`)
  abs <- p %>% select(sid = `dc:identifier`, `dc:description`) %>% filter(sid == p$`dc:identifier`)

  # TODO: raw org_sourcetext can contain a, b, "dagger" etc, may need cleaning
  sae <- scopus_abstract_extended(sid)
  abs <- sae$scopus_abstract
  cor <- sae$scopus_correspondence #%>% filter(if_all(.fns = function(x) !is.na(x)))
  ags <- sae$scopus_authorgroup #%>% filter(if_all(.fns = function(x) !is.na(x)))

  genres <- frag_genre(tolower(p$subtypeDescription))

  # TODO: this can also hold an external URL
  # '<location><url displayLabel="">http://..</url></location>'
  identifiers <- c(
    frag_identifier(type = "doi", identifier = p$`prism:doi`),
    frag_identifier(type = "scopus", identifier = p$eid),
    frag_identifier(type = "eissn", identifier = p$`prism:eIssn`), # should this be eIssn?
    frag_identifier(type = "issn", identifier = p$`prism:issn`),
    frag_identifier(type = "articleId", identifier = p$`article-number`)
  )

  guess_kthid <- function(my_orcid) {
    if (length(my_orcid) > 1 || is.na(my_orcid)) return (NA_character_)
    lookup <- kthid_orcid_lookup |> filter(orcid == my_orcid)
    if (nrow(lookup) == 0 || all(is.na(lookup))) return (NA_character_)
    lookup %>% pull(kthid)
  }

  #aff$orcid %>% map_chr(function(x) guess_kthid(orcid = x))
  #aff %>% pmap(.f = function(afid, ...) afid)

  if (is.null(pluck(ags, "orcid")))
    ags$orcid <- NA_character_

  # TODO: fix sae-fcn to include all authors and join affiliations with ;-sep
  # if role is an editor, an extra field has_role or so needs to be checked (if it exists)
  # "if a personal name with an affiliation is followed by a namepart with
  # a namepart element with that same exact name as the affiliation string, the affiliation
  # will be set at import"
  if (!"ce_given_name" %in% names(ags))
    ags$ce_given_name <- ags$preferred_name_ce_given_name
  if (!"raw_org" %in% names(ags))
    ags$raw_org <- ags$ce_text

  persons <-
    ags |>
    select(orcid, ce_surname, ce_given_name, seq, raw_org, afid) |>
    group_by(seq) |>
    summarize(across(where(is.character), function(x) {
        res <- paste0(collapse = "; ", unique(na.omit(x)))
        ifelse(all(res == ""), NA_character_, res)
      }
    )) |>
    ungroup() |>
    pmap(function(seq, orcid, ce_surname, ce_given_name, raw_org, afid, ...) {
      frag_name_personal(
        kthid = guess_kthid(my_orcid = orcid),
        source = "kth",
        family = ce_surname, given = ce_given_name,
        role = ifelse(seq == 1 , "aut", "aut"),
        affiliations = raw_org |> tidy_xml(cdata = TRUE),
        descriptions = if (all(is.na(orcid))) NULL else paste0("org.orcid=", orcid))
      }
    )

  # TODO: Can the publication status be picked up from Scopus?
  # One of Submitted / Accepted / In press / Published
  #<note type="publicationStatus" lang="eng">In press</note>

  #frag_publication_status(p$status)

  ext <- p$`prism:pageRange` |> scopus_extent_from_pagerange()

  related_item <- frag_relatedItem_host_journal(
    pub_title = p$`prism:publicationName`,
    pub_issn = p$`prism:issn`,
    pub_volume = p$`prism:volume`,
    pub_issue = p$`prism:issueIdentifier`,
    pub_extent_beg = ext["extent_beg"],
    pub_extent_end = ext["extent_end"]
  )

  # relateditems <-
  #   frag_relatedItem_series(
  #     title = p$`prism:publicationName`, eissn = p$`prism:eIssn`,
  #     issn = p$`prism:eIssn`, issue = p$`prism:issueIdentifier`
  #   )

  title <- frag_titleInfo(
    title = p$`dc:title` |> tidy_xml(), #subtitle = "Some subtitle",
    lang = "eng")

  origins <- frag_originInfo(
    publisher = sae$scopus_abstract$`dc:publisher`,
    yearIssued = lubridate::year(p$`prism:coverDate`),
    availableFrom = p$`prism:coverDisplayDate`
  )

  desc <- frag_physicalDescription(desc = "print")

  resourcetypes <- frag_typeOfResource(resourcetype = "text")

  notes <- c(
    frag_note("Imported from Scopus. VERIFY.")#,
    #frag_note("Another. @Funder@ [@project_number_from_funder@")
  )

  abstract <- frag_abstract(p$`dc:description` |> tidy_xml(cdata = TRUE))

  # both author keywords and UKÄ classifications are MODS "subjects"
  keywords <-
    p$authkeywords |>
    gsub(pattern = " [|] ", replacement = ", ") |>
    strsplit(", ") |>
    unlist()

  hsv_call <-
    classify_swepub(
      title = p$`dc:title`,
      keywords = p$`dc:description`,
      abstract = paste0(collapse = " ", keywords), level = "5"
    )

  if (nrow(hsv_call) == 0) {
    hsv_categories <- NULL
  } else {
    hsv_categories <-
      hsv_call |>
      select(-c("eng_topics", "swe_topics")) |>
      tidyr::pivot_longer(cols = ends_with(c("label")), names_to = "subject") |>
      tidyr::separate("subject", into=c("lang", NA)) |>
      mutate(source = "hsv", href = unique(eng_code, swe_code)) |>
      rename(topic = value) |>
      pmap(function(lang, source, href, topic, ...)
        frag_subject(lang, source, href, topic)
      )
  }

  subjects <- c(frag_subject(topic = keywords), hsv_categories)

  # TODO: how should this be used?
  location <- frag_location(p$`prism:url`)

  #series <- frag_relatedItem_series(title = "relseries",
  #  issn = "my_issn", eissn = "my_eissn", issue = "1")

  # TODO: continue here! Example with 10 publication or so...
  dmp <- diva_mods_params(
    genres = genres
    #, recordinfos =
    , identifiers = identifiers
    , persons = persons
    , subjects = subjects
    , relateditems = related_item
    , title = title
    , abstract = abstract
    , notes = notes
    , origins = origins
    , resourcetypes = resourcetypes
    , desc = desc
    , location = location
    #  , series =
  )

  dmp

}

#' Generate MODS from a Scopus identifier
#' @param sid Scopus identifier
#' @param scopus reference data for the identifier, by default from scopus_from_minion()
#' @param ko reference data for known KTH author identifiers' ORCiD associations
#' by default from kthid_orcid()
#' @export
#' @importFrom xml2 read_xml
scopus_mods <- function(sid, scopus = scopus_from_minio(), ko = kthid_orcid()) {
  my_params <- scopus_mods_params(scopus, sid, ko)
  my_mods <- create_diva_mods(my_params)
  res <- xml2::read_xml(my_mods) |> as.character()
  cat(res)
  invisible(res)
}

#' Generate MODS from several Scopus identifiers
#' @param sids Scopus identifier
#' @param scopus reference data for the identifier, by default from scopus_from_minion()
#' @param ko reference data for known KTH author identifiers' ORCiD associations
#' by default from kthid_orcid()
#' @export
#' @importFrom xml2 read_xml
scopus_mods_crawl <- function(sids, scopus = scopus_from_minio(), ko = kthid_orcid()) {

  ids <- unique(sids)

  pb <- progress::progress_bar$new(total = length(ids))
  message("Generating MODS parameters for ", length(ids), " identifiers...")

  smp <- function(x) {
    pb$tick()
    scopus_mods_params(scopus, x, ko)
  }

  my_params <-
    ids |> map(possibly(smp)) |>
    setNames(nm = ids) |> compact()

  failed_params <- setdiff(ids, names(my_params))

  if (length(failed_params) > 0)
    message("Failed to generate parameters for these ids: ",
      paste0(collapse = " ", sep = " ", failed_params)
    )

  message("Generating MODS based on parameters...")

  my_mods <-
    my_params |> map(possibly(.f = function(x)
      create_diva_mods(x) |> xml2::read_xml() |> as.character())) |>
    setNames(nm = ids) |> compact()

  # my_validations <-
  #   my_mods |> map(possibly(xml2::read_xml)) |>
  #   setNames(nm = ids) |> compact()

  failed_mods <- setdiff(names(my_mods), names(my_mods))

  if (length(failed_mods) > 0)
    message("Failed to generate valid MODS xml for these ids: ",
      paste0(collapse = " ", sep = " ", failed_mods))

  debug <- list(
    params = my_params,
    mods = my_mods,
    fails = unique(c(failed_mods, failed_params))
  )

  message("Returning ", length(my_mods), " MODS invisibly")

  if (length(debug$fails) > 0)
    warning("Failed conversion for these identifiers: ",
      paste0(collapse = " ", sep = " ", debug$fails))

  invisible(structure(my_mods, debug = debug))

}

#' Write a zipped MODS-file with results from a scopus mods crawl
#' @param crawl_result the results from scopus_mods_crawl()
#' @param path a path to a directory on disk
#' @param zipfile name of zip file, by default mods.zip
#' @importFrom readr write_file
#' @importFrom purrr map2
#' @importFrom zip zip
#' @export
write_mods_zip <- function(crawl_result, path = tempdir(), zipfile = "mods.zip") {

  fns <- crawl_result |> names() |>
    gsub(pattern = "SCOPUS_ID:", replacement = "") |>
    paste0(".mods.xml")

  fns <- file.path(path, fns)

  # create dir if it does not exist
  if (!dir.exists(path)) {
    message("Directory ", path, " does not exist, creating it...")
    dir.create(path, recursive = TRUE)
  }

  # for all these filenames, write the files
  res <- purrr::map2(fns, names(crawl_result),
    function(fn, mods) readr::write_file(x = crawl_result[[mods]], file = fn)
  )

  zf <- file.path(path, zipfile)
  message("Generating zip file at ", zf)
  zip::zip(root = path, zipfile = zipfile,
    files = file.path(path, dir(path, pattern = ".mods.xml"))
  )

  unlink(fns)
  stopifnot(file.exists(zf))

  return(zf)

}
