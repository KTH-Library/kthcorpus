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
#' @importFrom utils tail
#' @export
scopus_mods_params <- function(scopus, sid, kthid_orcid_lookup = kthid_orcid()) {

  `dc:identifier` <- `dc:description` <-
    `given-name` <- surname <- authid <- auid <-
    ce_surname <- ce_given_name <-
    preferred_name_ce_given_name <- preferred_name_ce_surname <-
    raw_org <- afid <-
    eng_code <- swe_code <- value <-
    term <- sn <- rowid <- score <- enrich <- enrich_orcid <- NULL

  #sid <- "SCOPUS_ID:85136096142"

  # use info primarily from scopus Search API
  p <- scopus$publications |> filter(grepl(sid, `dc:identifier`))

  if (nrow(p) < 1)
    stop("The scopus identifier ", sid, " is not in the scopus search results")

  #abs <- p |> select(sid = `dc:identifier`, `dc:description`)

  # enable join on auid from Scopus Extended Search API
  aut <- scopus$authors |> filter(sid == p$`dc:identifier`) |>
    mutate(auid = as.character(authid))

  aff <- scopus$affiliations |> filter(sid == p$`dc:identifier`)


  # TODO: raw org_sourcetext can contain a, b, "dagger" etc, may need cleaning
  sae <- scopus_abstract_extended(sid)
  abs <- sae$scopus_abstract
  cor <- sae$scopus_correspondence
  ags <- sae$scopus_authorgroup |> mutate(seq = as.integer(seq)) |> arrange(-desc(seq))

  genres <- frag_genre2(p$`prism:aggregationType`, p$subtypeDescription)
  genre <- names(genres)

  # TODO: this can also hold an external URL
  # '<location><url displayLabel="">http://..</url></location>'
  identifiers <- c(
    frag_identifier(type = "doi", identifier = p$`prism:doi`),
    frag_identifier(type = "scopus", identifier = p$eid),
    frag_identifier(type = "eissn", identifier = p$`prism:eIssn`), # should this be eIssn?
    frag_identifier(type = "issn", identifier = p$`prism:issn`),
    frag_identifier(type = "articleId", identifier = p$`article-number`)
  )

  # TODO: fix parsing of `prism:isbn` "dictionary", esp "" should be NA

  isbn_identifiers <-
    as.character(p$`prism:isbn`) |> strsplit(split = " ") |> unlist() |>
    map_chr(function(x) frag_identifier(type = "isbn", displayLabel = "Undefined", identifier = x))

  notes <- c(
    frag_note(sprintf("QC %s \nImported from Scopus. VERIFY.\n", format(Sys.Date(), "%Y%m%d")))#,
    #frag_note("Another. @Funder@ [@project_number_from_funder@")
  )

  if (! genre %in% c("chapter", "conferencePaperPublished", "articleConferencePaper")) {
    identifiers <- c(identifiers, isbn_identifiers)
  } else {
    # TODO: isbn info should be used in Notes (Anders) for chapter and conferencePaperPublished
    # and articleConferencePaper
    # "partOf isbn"
    notes <- c(notes,
      frag_note(sprintf("Part of ISBN %s", p$`prism:isbn`))
    )
  }

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

  # TODO: if unique match then "£££[score=]" (surname) - but populate w more info
  # NB: it seems the scopus search api restricts the number of authors to 100
  # but only there do we get the orcid (?)

  sae_n_authors <-
    sae$scopus_authors |> select(-c("id", "i")) |> distinct() |> nrow()

  notes <-
    c(notes, glue::glue('<note type="creatorCount">{sae_n_authors}</note>',
      .na = ""))

  #TODO: to get orcid for sae_authors, join for at most 100 of those?

#  scopus_orcids <-

  sae_authors <- sae$scopus_authors

  authorz <-
    sae_authors |>
      group_by(auid) |>
      distinct(preferred_name_ce_surname, preferred_name_ce_given_name) |>
      ungroup() |>
      left_join(by = "auid", relationship = "many-to-many",
        ags |> select(seq, raw_org, seq, auid) |>
        group_by(seq) |>
        summarize(across(where(is.character), function(x) {
            res <- paste0(collapse = "; ", unique(na.omit(x)))
            ifelse(all(res == ""), NA_character_, res)
          }
        )) |> ungroup()
      ) |>
    select(-auid) |>
    tibble::rowid_to_column() |>
    mutate(orcid = NA) |>
    rename(
      surname = preferred_name_ce_surname,
      `given-name` = preferred_name_ce_given_name
    ) |>
    arrange(as.integer(seq))

  # authors <-
  #   aut |>
  #     group_by(auid) |>
  #     distinct(orcid, surname, `given-name`, surname) |>
  #     ungroup() |>
  #     left_join(by = "auid",
  #       ags |> select(seq, raw_org, seq, auid) |>
  #       group_by(seq) |>
  #       summarize(across(where(is.character), function(x) {
  #           res <- paste0(collapse = "; ", unique(na.omit(x)))
  #           ifelse(all(res == ""), NA_character_, res)
  #         }
  #       )) |> ungroup()
  #     ) |>
  #   select(-auid) |>
  #   tibble::rowid_to_column()

  # TODO: for many scopusids, get the raw_org... find all with afid for KTH
  # run the re_kth - does it match for all those records.
  re_kth <- paste0(
    "kth|roy.*?inst.*?tech.*?|roy\\. inst\\. t|alfven|",
    "kung.*?tek.*?h[o\u00f6]g.*?|kgl.*?tek.*?|kung.*?tek.*?hg.*?|roy.*?tech.*?univ.*?"
  )

  my_authorz <- authorz

  authors_kth <- my_authorz |> filter(grepl(re_kth, tolower(raw_org))) |> rowwise() |>
    mutate(term = glue::glue(.na = "", .sep = " ", orcid, `given-name`, surname))

  if (sae_n_authors >= 30) {
    author_first <- my_authorz |> head(1)
    author_last <- my_authorz |> tail(1)
    my_authorz <- bind_rows(author_first, authors_kth, author_last)
  }

  suggestions <- authors_kth

  if (nrow(suggestions) > 0) {
    suggestions <-
      suggestions |> mutate(sn = list(search_names(term))) |>
      mutate(enrich = list(sn |>
        group_by(rowid) |> arrange(desc(score)) |> filter(score > 10) |>
        summarise(
          across(where(is.character), function(x) paste0(collapse = " ", unique(na.omit(x)))),
          score = mean(score),
          n = n_distinct(score)
        ) |>
         select(any_of(c("orcid", "kthid", "fullname", "orgid", "score", "n"))) #|>
        )) |>
      unnest(enrich, names_sep = "_") |>
      select("rowid", starts_with("enrich")) |>
      mutate(across(where(is.character), function(x) na_if(x, ""))) |>
      rowwise() |>
      mutate(enrich_orcid = strsplit(enrich_orcid, "\\s+") |> unlist() |> toupper() |> unique() |>  paste(collapse = " "))
  } else {
    suggestions <-
      data.frame() |> as_tibble() |>
      mutate(rowid = NA, enrich_kthid = NA, enrich_orcid = NA)
  }

  enriched <-
    my_authorz |> left_join(suggestions, by = "rowid") |>
    #filter(!is.na(enrich_kthid))
    mutate(surname = ifelse(grepl(re_kth, tolower(raw_org)), paste0("$$$", surname), surname))

  persons <-
    # use "given-name" and "surname" from search API (aff)
    # make seq numeric for proper sorting
    enriched |>
    pmap(function(seq, orcid, surname, `given-name`, raw_org,
                  enrich_orcid, enrich_kthid, enrich_orgid, ...) {
      frag_name_personal(
        kthid = enrich_kthid,
        source = "kth",
        family = surname,
        given = `given-name`,
        role = ifelse(seq == 1 , "aut", "aut"),
        affiliations = raw_org |> tidy_xml(cdata = TRUE),
        # give orcid precendence over enrich_orcid if both exist
        descriptions = if (all(is.na(orcid), is.na(enrich_orcid))) NULL else paste0("orcid.org=", ifelse(is.na(orcid), enrich_orcid, orcid)))
    })



  # TODO: Can the publication status be picked up from Scopus?
  # One of Submitted / Accepted / In press / Published
  #<note type="publicationStatus" lang="eng">In press</note>

  #frag_publication_status(p$status)

  ext <- p$`prism:pageRange` |> scopus_extent_from_pagerange()

  related_item_journal <- frag_relatedItem_host_journal(
    pub_title = p$`prism:publicationName`,
    pub_issn = p$`prism:issn`,
    pub_eissn = p$`prism:eIssn`,
    pub_volume = p$`prism:volume`,
    pub_issue = p$`prism:issueIdentifier`,
    pub_extent_beg = ext["extent_beg"],
    pub_extent_end = ext["extent_end"]
  )

  related_item_conf <- sae$scopus_confinfo |> pmap(frag_relatedItem_conference)

  if (genre %in% c("conferencePaperPublished", "articleConferencePaper", "conferenceProceedings")) {
    related_items <- related_item_conf
    # TODO FIXME: Special case introduced for "Conference Proceeding" / "Book Chapter"
    if (length(related_item_conf) == 0)
      related_items <- related_item_journal
  } else {
    related_items <- related_item_journal
  }

  # related_item_series <-
  #   frag_relatedItem_series(
  #     title = p$`prism:publicationName`,
  #     eissn = p$`prism:eIssn`,
  #     issn = p$`prism:issn`,
  #     issue = p$`prism:issueIdentifier`
  #   )

  title <- frag_titleInfo(
    title = p$`dc:title` |> tidy_xml(), #subtitle = "Some subtitle",
    lang = "eng")

  origins <- frag_originInfo(
    publisher = sae$scopus_abstract$`dc:publisher` |> tidy_xml(),
    yearIssued = lubridate::year(p$`prism:coverDate`),
    availableFrom = p$`prism:coverDisplayDate`
  )

  desc <- frag_physicalDescription(desc = "print")

  resourcetypes <- frag_typeOfResource(resourcetype = "text")

  abstract <- frag_abstract(p$`dc:description` |> tidy_xml(cdata = TRUE))

  # both author keywords and classifications are MODS "subjects"
  keywords <-
    p$authkeywords |>
    gsub(pattern = " [|] ", replacement = ", ") |>
    strsplit(", ") |>
    unlist()

  if (all(is.na(keywords))) keywords <- NULL

  hsv_call <-
    classify_swepub(
      title = p$`dc:title`,
      abstract = p$`dc:description`,
      keywords = paste0(collapse = " ", keywords),
      level = "5"
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

  # TODO: how should this be used? Remove it for now.
  location <- NULL #frag_location(p$`prism:url`)

  #series <- frag_relatedItem_series(title = "relseries",
  #  issn = "my_issn", eissn = "my_eissn", issue = "1")

  # TODO: continue here! Example with 10 publication or so...
  dmp <- diva_mods_params(
    genres = genres
    #, recordinfos =
    , identifiers = identifiers
    , persons = persons
    , subjects = subjects
    , relateditems = related_items
    , title = title
    , abstract = abstract
    , notes = notes
    , origins = origins
    , resourcetypes = resourcetypes
    , desc = desc
    , location = location
    #, series = related_item_series
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
  res <- my_mods #xml2::read_xml(my_mods) |> as.character()
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
    ids |> map(possibly(smp), .progress = TRUE) |>
    setNames(nm = ids) |> compact()

  failed_params <- setdiff(ids, names(my_params))

  if (length(failed_params) > 0)
    message("Failed to generate parameters for these ids: \n",
      paste0(collapse = "\n", sep = " ", failed_params)
    )

  message("Generating MODS based on parameters...")

  my_mods <-
    my_params |> map(possibly(.f = function(x)
      create_diva_mods(x)), .progress = TRUE) |> # xml2::read_xml() |> as.character())) |>
    setNames(nm = names(my_params)) |> compact()

  my_validations <-
    my_mods |> map(possibly(xml2::read_xml), .progress = TRUE) |>
    setNames(nm = names(my_mods)) |> compact()

  failed_mods <- setdiff(ids, names(my_validations))

  if (length(failed_mods) > 0)
    message("Failed to generate valid MODS xml for these ids: \n",
      paste0(collapse = "\n", sep = " ", failed_mods))

  debug <- list(
    params = my_params,
    mods = my_mods,
    fails = unique(c(failed_mods, failed_params))
  )

  message("Returning ", length(my_mods), " MODS invisibly")

  if (length(debug$fails) > 0)
    warning("Failed conversion for these identifiers:\n",
      paste0(collapse = "\n", sep = " ", debug$fails))

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
    files = file.path(path, dir(path, pattern = ".mods.xml")),
    mode = "cherry-pick"
  )

  unlink(fns)
  stopifnot(file.exists(zf))

  return(zf)

}

#' Write MODS collections as XML files, where each contains a certain number of MODS
#'
#' @param mods a vector of MODS generated from scopus_mods_crawl()
#' @param outdir the directory to write the files
#' @param prefix the pattern to prefix file names with, by default "mods"
#' @param chunk_size the number of MODS in each batch, by default 25
#' @export
#' @importFrom purrr walk2
write_mods_chunked <- function(mods, outdir = ".", prefix = "mods", chunk_size = 25) {
  chunks <- mods |> split(ceiling(seq_along(mods) / chunk_size))
  fns <- file.path(outdir, paste0(sprintf("%s_%02d", prefix, as.integer(names(chunks))), ".xml"))
  message("Chunk lengths: ", paste0(collapse = " ", lengths(chunks)))
  message(paste0(collapse = "\n", fns))
  purrr::walk2(chunks, fns, function(x, y) write_file(x |> create_diva_modscollection(), y))
}
