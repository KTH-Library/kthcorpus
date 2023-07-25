#' Potential data quality issues for DiVA author data
#'
#' This function summarizes some potential data quality issues for DiVA
#' author data, for example records with ORCIDs that relates to multiple
#' KTH author identifiers and vice versa. Some of these records may be
#' candidates for merging author data at the source.
#'
#' @param authors a tibble with authors data, default: kth_diva_authors()
#' @return a list with slots for data frames (details, overview, and associated
#' publications)
#' @export
#' @import dplyr
kth_diva_issues <- function(authors = kth_diva_authors()) {
  namez <- authors

  # orcids which relates to more than one kthid
  # these could be data quality issues?
  details <-
    namez |>
    filter(!is.na(orcid) & !is.na(kthid)) |>
    group_by(orcid) |>
    mutate(n1 = n_distinct(name), n2 = n_distinct(kthid)) |>
    filter(n2 > 1) |>
    distinct(orcid, kthid, n2, name) |>
    ungroup() |>
    arrange(orcid, kthid, desc(n2), name) |>
    select(orcid, kthid, n = n2) |>
    inner_join(namez) |>
    distinct(orcid, kthid) |>
    collect()

  overview <-
    details |>
    group_by(orcid) |>
    count(kthid) |>
    arrange(orcid, desc(n)) |>
    collect()

  # publications for kthids that have several different orcids
  pubs <-
    namez |>
    left_join(namez |>
                filter(!is.na(orcid) & !is.na(kthid)) |>
                distinct(orcid, kthid), by = "kthid") |>
    filter(orcid.x != orcid.y) |>
    select(kthid, orcid.x, orcid.y, everything()) |>
    arrange(desc(kthid)) |>
    collect()

  list(overview = overview, details = details, pubs = pubs)
}

paste_na <- function(x, y)
  tidyr::unite(tibble::tibble(x = x, y = y), "xy", x, y, na.rm = TRUE)$xy

blank <- function(x) ifelse(is.na(x), "", x)

link_general <- function(href, text) {

  if (missing(text)) text <- href

  if (!nzchar(href) || !nzchar(text))
    return (NA_character_)

  sprintf("<a href='%s' target='_blank' rel='noopener noreferrer' title='%s'>%s</a>",
          blank(href), blank(text), blank(shorten(text))
  )

}


#' Convert a string with a URL to a link
#'
#' @param href the link
#' @param text the text to show for the link, by default shortened href
#' @param title the tooltip, by default the href
#' @param target the type of link to make
#' @importFrom stringr str_c
#' @export
linkify <- function(href, text = shorten(href), title = href, target =
  c("href", "PID", "titlesearch", "freetextsearch", "DOI", "ScopusID", "ISSN",
    "ISBN", "ISI", "ORCID", "PMID")) {

  if (missing(target)) target <- "href"

  pattern <-
    "<a href='%s' target='_blank' rel='noopener noreferrer' title='%s'>%s</a>"

  p_pattern <- function(x, y, z) {
    stringr::str_c(
      "<a href='",
      x,
      "' target='_blank' rel='noopener noreferrer' title='",
      y,
      "'>",
      z,
      "</a>"
      )
  }

  portal <- diva_config()$portal

  p_PID <- function(x) {
    stringr::str_c(
      paste0(portal, "/smash/record.jsf?dswid=-310&pid=diva2%3A"),
      x
    )
  }

  p_ISSN <- function(x) {
    stringr::str_c(
      "https://portal.issn.org/api/search?search[]=MUST=allissnbis=%22",
      x, "%22")
  }

  p_ScopusID <- function(x) {
    stringr::str_c(
      "https://www.scopus.com/record/display.url?origin=inward&partnerID=40&eid=",
      x
    )
  }

  p_DOI <- function(x) {
    stringr::str_c(
      "https://doi.org/",
      x
    )
  }

  search_title <- function(term)
    stringr::str_c(
      paste0(portal, "/smash/resultList.jsf"),
      "?aq2=%5B%5B%5D%5D&af=%5B%5D&searchType=RESEARCH",
      "&sortOrder2=title_sort_asc&language=en",
      "&aq=%5B%5B%7B%22titleAll%22%3A%22",
      utils::URLencode(term, reserved = TRUE, repeated = FALSE),
      "%22%7D%5D%5D",
      "&sf=all&aqe=%5B%5D&sortOrder=author_sort_asc",
      "&onlyFullText=false&noOfRows=50&dswid=-4347"
    )

  search_free <- function(term)
    stringr::str_c(
      paste0(portal, "/smash/resultList.jsf"),
      "?language=en&searchType=RESEARCH&",
      "query=&af=%5B%5D&aq=%5B%5B%7B%22freeText%22%3A%22",
      utils::URLencode(term, reserved = TRUE, repeated = FALSE),
      "%22%7D%5D%5D&aq2=%5B%5B%5D%5D&aqe=%5B%5D&noOfRows=50",
      "&sortOrder=author_sort_asc&sortOrder2=title_sort_asc",
      "&onlyFullText=false&sf=all"
    )

  p_ISBN <- function(x) {
    # WorldCat-sökning på ISBN:
    # https://www.worldcat.org/search?q=bn%3A978-91-7501-918-5&qt=advanced&dblist=638
    stringr::str_c(
      "https://www.worldcat.org/search?q=bn%3A",
      x,
      "&qt=advanced&dblist=638"
    )
  }

  p_ISI <- function(x) {
    # Web of Science, sökning på UT=WoS-id:
    # http://apps.webofknowledge.com/InboundService.do?product=WOS&action=retrieve&mode=FullRecord&UT=000722432000004
    # New URL: https://www.webofscience.com/wos/woscc/full-record/WOS:001001819900001
    stringr::str_c(
      "https://www.webofscience.com/wos/woscc/full-record/WOS:",
      x
    )
  }

  p_ORCID <- function(x) {
    #sprintf("https://orcid.org/%s", blank(orcid)), orcid))
    stringr::str_c("https://orcid.org/", x)
  }

  p_PMID <- function(x) {
    stringr::str_c("https://pubmed.ncbi.nlm.nih.gov/", x)
  }

  href_modified <- switch (target,
    href = href,
    PID = p_PID(href),
    titlesearch = search_title(href),
    freetextsearch = search_free(href),
    DOI = p_DOI(href),
    ScopusID = p_ScopusID(href),
    ISSN = p_ISSN(href),
    ISBN = p_ISBN(href),
    ISI = p_ISI(href),
    ORCID = p_ORCID(href),
    PMID = p_PMID(href),
    href
  )

  p_pattern(
    href_modified, title, text
  )

}

link_diva <- function(href, text) {
  if ((length(href) == 1 || length(text) == 1) && all(!nzchar(href) | !nzchar(text)))
    return (NA_character_)
  paste0(paste0("<a href='", diva_config()$portal, "/smash/record.jsf?dswid=-310&pid=diva2%3A"),
         href, "' target='_blank' rel='noopener noreferrer' title='", text, "'>", shorten(text), "</a>")
}

link_DOI <- function(href, text) {
  if ((length(href) == 1 || length(text) == 1) && all(!nzchar(href) | !nzchar(text)))
    return (NA_character_)
  paste0("<a href='https://doi.org/", href,
     "' target='_blank' rel='noopener noreferrer' title='", text, "'>", shorten(text), "</a>")
}

link_ScopusID <- function(href, text) {
  if ((length(href) == 1 || length(text) == 1) && all(!nzchar(href) | !nzchar(text)))
    return (NA_character_)
  paste0("<a href='https://www.scopus.com/record/display.url?origin=inward&partnerID=40&eid=", href,
         "' target='_blank' rel='noopener noreferrer' title='", text, "'>", shorten(text), "</a>")
}

#' @importFrom utils URLencode
link_titlesearch <- function(title, text) {

  if (missing(text)) text <- title

  if ((length(title) == 1 || length(text) == 1) && all(!nzchar(title) | !nzchar(text)))
    return (NA_character_)

  title_search <- function(term)
    paste0("https://kth.diva-portal.org/smash/resultList.jsf",
           "?aq2=%5B%5B%5D%5D&af=%5B%5D&searchType=RESEARCH",
           "&sortOrder2=title_sort_asc&language=en",
           "&aq=%5B%5B%7B%22titleAll%22%3A%22", utils::URLencode(term), "%22%7D%5D%5D",
           "&sf=all&aqe=%5B%5D&sortOrder=author_sort_asc&onlyFullText=false&noOfRows=50&dswid=-4347")

  sprintf("<a href='%s' target='_blank' rel='noopener noreferrer' title='%s'>%s</a>",
          title_search(title), text, shorten(text))

}

link_freetextsearch <- function(title, text) {

  if (missing(text)) text <- title

  if (!nzchar(title) || !nzchar(text))
    return (NA_character_)

  ft_search <- function(term)
    paste0("https://kth.diva-portal.org/smash/resultList.jsf",
           "?language=en&searchType=RESEARCH&",
           "query=&af=%5B%5D&aq=%5B%5B%7B%22freeText%22%3A%22",
           utils::URLencode(term),
           "%22%7D%5D%5D&aq2=%5B%5B%5D%5D&aqe=%5B%5D&noOfRows=50",
           "&sortOrder=author_sort_asc&sortOrder2=title_sort_asc",
           "&onlyFullText=false&sf=all")

  sprintf("<a href='%s' target='_blank' rel='noopener noreferrer' title='%s'>%s</a>",
          blank(ft_search(title)), blank(text), blank(shorten(text)))
}

link_ISSNsearch <- function(title, text) {

  if (missing(text)) text <- title

  if (!nzchar(title) || !nzchar(text))
    return (NA_character_)

  issn_search <- function(term)
    paste0("https://portal.issn.org/api/search?search[]=MUST=allissnbis=%22", term, "%22")

  sprintf("<a href='%s' target='_blank' rel='noopener noreferrer' title='%s'>%s</a>", issn_search(title), text, shorten(text))

}

shorten <- function(x, w = 25) {
  stringr::str_trunc(x, width = w)
  #ifelse(nchar(x) > 20, paste0(substr(x, 1, 20), "..."), x)
}

check_multiplettes_article_title <- function(pubs = kth_diva_pubs(), authors = kth_diva_authors()) {

  # NB: this check is not currently included in report
  Year <- n_check <- LastUpdated <- lc_title <- NULL

  article_title_multiplettes <-
    pubs |>
    filter(grepl("^Artikel", PublicationType)) |>
    mutate(lc_title = trimws(tolower(Title))) |>
    group_by(lc_title) |>
    count(lc_title) |>
    arrange(desc(n)) |>
    filter(n > 1)

  atm <-
    article_title_multiplettes |>
    left_join(
      pubs |>
        filter(grepl("^Artikel", PublicationType)) |>
        mutate(lc_title = trimws(tolower(Title))) |>
        select(Title, lc_title, PID)
      , by = "lc_title") |>
    group_by(lc_title, n) |>
    summarize(pids = paste0(collapse = " ", PID)) |>
    arrange(desc(n)) |>
    collect()

  # FIXME

  atm <-
    atm |> tidyr::separate_rows(pids) |>
    mutate(PID = as.double(pids)) |>
    inner_join(
      pubs |> mutate(lc_title = trimws(tolower(Title)))
      , by = c("lc_title", "PID")
    ) |>
    #mutate(year = lubridate::year(PublicationDate)) |>
    select(Title, lc_title, n, PID, Year, LastUpdated)

  a <-
    authors |>
    filter(PID %in% atm$PID) |>
    mutate(last_name = gsub("(.+?)(,.*)", "\\1", name)) |>
    group_by(PID) |>
    summarise(initials = paste0(collapse = "", substr(last_name, 1, 1)))

  atm |>
    inner_join(a, by = "PID") |>
    mutate(check_key = paste0(initials, "_", Year)) |>
    select(PID, Year, Title, lc_title, LastUpdated, check_key, n) |>
    mutate(PID = link_diva(PID, PID)) |>
    mutate(Title = link_titlesearch(Title, Title)) |>
    group_by(check_key) |>
    add_count(check_key, sort = TRUE, name = "n_check") |>
    arrange(desc(LastUpdated), desc(check_key), desc(n_check), desc(Title)) |>
    ungroup() |>
    collect() |>
    select(PID, Year, Title, check_key, n, n_check, LastUpdated)

}

#' Clean HTML tags from string
#' @param x the string to clean out HTML tags from
#' @export
#' @importFrom rvest read_html html_text
tidy_html <- function(x)
  rvest::html_text(rvest::read_html(charToRaw(x)))


#' @importFrom methods formalArgs
prefilter_pubs <- function(fn) {

  CreatedDate <- NULL

  if ("pubs" %in% formalArgs(fn)) {
    formals(fn)$pubs <-
      kth_diva_pubs() |> filter(grepl("QC ", Notes), CreatedDate < as.Date(Sys.Date() - 30))
  }

  fn()
}

check_multiplettes_title <- function(pubs = kth_diva_pubs(), aut = kth_diva_authors()) {

  Year <- LastUpdated <- clean_notes <- n_check <- lc_title <- NULL

  re <- paste0(
    "Not duplicate with|not duplicate with|Non-duplicate with|Not dublicate|No duplicate",
    "|Not a duplicate with|No duplikate|Nondupe with|Non-duplicate in",
    "|No dublicate with|No dupe"
  )

  title_multiplettes <-
    pubs |>
    mutate(clean_notes = map_chr(Notes, tidy_html)) |>
    mutate(lc_title = trimws(tolower(Title))) |>
    filter(!grepl(re, clean_notes)) |>
    #  filter(grepl("^Artikel", PublicationType)) |>
    group_by(lc_title, PublicationType) |>
    count(lc_title) |>
    arrange(desc(n)) |>
    filter(n > 1)

  tm <-
    title_multiplettes |>
    left_join(
      pubs |>
        mutate(clean_notes = map_chr(Notes, tidy_html)) |>
        mutate(lc_title = trimws(tolower(Title))) |>
        filter(!grepl(re, clean_notes)) |>
        select(Title, lc_title, PID, PublicationType, DOI)
      , by = c("lc_title", "PublicationType")) |>
    group_by(lc_title, n, PublicationType) |>
    summarize(pids = paste0(collapse = " ", PID), .groups = "drop") |>
    arrange(desc(n)) |>
    collect()

  tm <-
    tm |>
    tidyr::separate_rows(pids) |>
    mutate(PID = as.double(pids)) |>
    inner_join(
      pubs |> mutate(lc_title = trimws(tolower(Title))),
      by = c("lc_title", "PID", "PublicationType")) |>
    #mutate(year = lubridate::year(PublicationDate)) |>
    select(Title, n, PID, Year, LastUpdated, PublicationType, Notes, JournalISSN, JournalEISSN) |>
    mutate(clean_notes = map_chr(Notes, tidy_html)) |>
#    count(JournalISSN, JournalEISSN)
    filter(JournalISSN == JournalEISSN | any(is.na(c(JournalISSN, JournalEISSN))))

  a <-
    aut |>
    filter(PID %in% tm$PID) |>
    mutate(last_name = gsub("(.+?)(,.*)", "\\1", name)) |>
    group_by(PID) |>
    summarise(initials = paste0(collapse = "", substr(last_name, 1, 1)))

  # NOTE: using non ascii characters here -> stringi::stri_escape_unicode can help!
  exceptions <- trimws(readr::read_lines(
    "Background
    Commentary
    Conclusions
    Conclusion
    Editorial
    Editor's foreword
    Editors' foreword
    Editors\u2019 foreword
    Examples
    Foreword
    F\u00f6rord
    Guest editorial
    Introduction
    Message from the [Cc]hairs
    Preface
    Preliminaries
    Preview
    Untitled")) |> paste0(collapse  = "|")

  b <-
    tm |>
    inner_join(a, by = "PID") |>
    mutate(check_key = paste0(initials, "_", Year)) |>
    select(PID, Year, Title, PublicationType, LastUpdated, check_key, n, JournalISSN, JournalEISSN, clean_notes) |>
    group_by(check_key) |>
    add_count(check_key, sort = TRUE, name = "n_check") |>
    arrange(desc(n_check), desc(check_key), desc(Title), desc(LastUpdated)) |>
    ungroup() |>
    filter(!grepl(exceptions, Title))


  stoplist <- checks_exclusions()

  b |> anti_join(stoplist, by = c("PID", "Title")) |>
    mutate(PID = linkify(PID, target = "PID")) |>
    mutate(Title = linkify(Title, target = "titlesearch")) |>
    collect() |>
    select(PID, Year, Title, PublicationType, n_check, n, LastUpdated, JournalISSN, JournalEISSN, check_key, clean_notes)

}

check_invalid_submission_status <- function(pubs = kth_diva_pubs()) {

  # possibly a Zenodo DOI could exist, but in general no identifier
  # has yet been assigned if "only" submitted

  ScopusId <- ISI <- DOI <- PID <- Year <- NULL

  pubs |>
    filter(Status == "submitted" &
        (!is.na(DOI) | !is.na(ISI) | !is.na(ScopusId))) |>
    select(PID, Year, ISI, DOI, ScopusId) |>
    mutate(
      PID = linkify(PID, target = "PID"),
      ISI = linkify(ISI, target = "ISI"),
      DOI = linkify(DOI, target = "DOI"),
      ScopusId = linkify(ScopusId, target = "ScopusID")
    )

}

check_missing_kthid <- function(authors = kth_diva_authors(), pubs = kth_diva_pubs()) {

  LastUpdated <- Year <- orgid <- ScopusId <- NULL

  authors |>
    filter(!is.na(orgid) & is.na(kthid))  |>
    inner_join(pubs |> select(PID, Year, LastUpdated), by = "PID") |>
    mutate(PID = linkify(PID, target = "PID")) |>
#    mutate(pids = linkify(pids, target = "freetextsearch")) |>
    mutate(name = linkify(name, target = "freetextsearch")) |>
    mutate(orgid = linkify(orgid, target = "freetextsearch")) |>
    mutate(extorg = linkify(extorg, target = "freetextsearch")) |>
    mutate(orcid = linkify(orcid, target = "ORCID")) |>
    mutate(ScopusId = linkify(ScopusId, target = "ScopusID")) |>
    mutate(DOI = linkify(DOI, target = "DOI")) |>
    arrange(desc(LastUpdated)) |>
    select(PID, Year, name, LastUpdated, kthid, orcid, DOI, ScopusId)

}

check_missing_aid <- function(authors = kth_diva_authors(), pubs = kth_diva_pubs()) {

  LastUpdated <- Year <- orgid <- ScopusId <- NULL

  authors |>
    filter(!is.na(orgid) & is.na(kthid))  |>
    inner_join(pubs |> select(PID, Year, LastUpdated), by = "PID") |>
    mutate(PID = linkify(PID, target = "PID")) |>
    #    mutate(pids = linkify(pids, target = "freetextsearch")) |>
    mutate(name = linkify(name, target = "freetextsearch")) |>
    mutate(orgid = linkify(orgid, target = "freetextsearch")) |>
    mutate(extorg = linkify(extorg, target = "freetextsearch")) |>
    mutate(orcid = linkify(orcid, target = "ORCID")) |>
    mutate(ScopusId = linkify(ScopusId, target = "ScopusID")) |>
    mutate(DOI = linkify(DOI, target = "DOI")) |>
    arrange(desc(LastUpdated)) |>
    select(PID, Year, name, LastUpdated, aid = kthid, orcid, DOI, ScopusId)

}


check_missing_orcids <- function(authors = kth_diva_authors()) {

  kthid <- orcid <- PID <- nd <- nd_casing <- has_casing <-
    na <- orcids <- NULL

  authors |>
    select(kthid, orcid, PID) |>
    filter(!is.na(kthid), !(grepl("00000*|-", kthid))) |>
    group_by(kthid) |>
    summarise(
      na = sum(is.na(orcid)),
      nd = n_distinct(orcid, na.rm = TRUE),
      nd_casing = n_distinct(toupper(orcid), na.rm = TRUE),
      orcids = paste0(collapse = ", ", na.omit(unique(orcid))),
    ) |>
    mutate(
      has_casing = (nd != nd_casing)
    ) |>
    arrange(desc(nd)) |>
    select(kthid, na, nd, nd_casing, has_casing, orcids) |>
    filter(nd == 1) |>
    arrange(desc(na)) |>
    filter(na > 9) |>
    mutate(cs = cumsum(na)) |>
    select(kthid, orcid = orcids, n_na_pubs = na) |>
    mutate(orcid = linkify(orcid, target = "ORCID"))

}

check_missing_date <- function(pubs = kth_diva_pubs()) {
  # TODO: PublicationDate may not be needed (earlier such issues are fixed)
  Year <- NULL
  pubs |>
    #filter(PublicationType == "Konferensbidrag") |>
    filter(is.na(PublicationDate), is.na(Year))  |>
    collect() |>
    select(-any_of(c("ISRN", "NBN")))
}

check_missing_journals_identifiers <- function(pubs = kth_diva_pubs()) {

  ScopusId <- Year <- Journal <- LastUpdated <- clean_notes <-
    Status <- PublicationType <- NULL

  step <-
    pubs |>
    filter(PublicationType == "Artikel i tidskrift") |>
    filter(is.na(JournalISSN) & is.na(JournalEISSN))  |>
    collect()

  step |>
    mutate(PID = link_diva(PID, PID)) |>
    mutate(Title = linkify(Title, target = "titlesearch")) |>
    mutate(DOI = linkify(DOI, target = "DOI")) |>
    mutate(ScopusId = linkify(ScopusId, target = "ScopusID")) |>
    mutate(Name = linkify(Name, target = "freetextsearch")) |>
    mutate(Journal = linkify(Journal, target = "freetextsearch")) |>
    mutate(clean_notes = map_chr(Notes, tidy_html)) |>
    filter(!grepl("No ISSN", clean_notes)) |>
    filter(!Status %in% c("submitted", "accepted")) |>
    select(PID, Year, Title, LastUpdated, starts_with("Journal"), PublicationType, Status, DOI, ScopusId, Name, clean_notes)
}

check_titles_book_chapters <- function(pubs = kth_diva_pubs()) {

  t1 <-
    pubs |>
      filter(PublicationType == "Kapitel i bok, del av antologi")

  t1 |>
    group_by(Title) |>
    count(Title) |>
    filter(n > 1, nchar(Title) < 20) |>
    arrange(desc(n)) |>
    inner_join(t1 |> select(PID, Title), by = "Title") |>
    group_by(Title, n) |>
    summarize(pids = paste0(collapse = " ", PID))  |>
    collect()

}

check_invalid_ISI <- function(pubs = kth_diva_pubs()) {

  # from kth-library/bibliutils: r'A19\d{2}[A-Z\d]{5}\d{5}|000\d{12}'
  re <- "^A19\\d{2}[A-Z\\d]{5}\\d{5}$|^000\\d{12}$|^001\\d{12}$"

  ISI <- PID <- Year <- DOI <- ScopusId <- NULL

  pubs |>
    filter(!is.na(ISI)) |>
    #filter(nchar(ISI) != 15, !grepl("^A1", ISI), !grepl("^000", ISI))  |>
    filter(!grepl(re, ISI, perl = TRUE)) |>
    collect() |>
    select(PID, Year, ISI, DOI, ScopusId) |>
    mutate(
      PID = linkify(PID, target = "PID"),
      ISI = linkify(ISI, target = "ISI"),
      DOI = linkify(DOI, target = "DOI"),
      ScopusId = linkify(ScopusId, target = "ScopusID")
    )

}

check_invalid_DOI <- function(pubs = kth_diva_pubs()) {

  ISI <- PID <- Year <- DOI <- ScopusId <- NULL

  pubs |>
    filter(!is.na(DOI)) |>
    filter(!grepl("^10[.]", DOI)) |>
    select(PID, Year, DOI, ISI, ScopusId)  |>
    mutate(
      PID = linkify(PID, target = "PID"),
      ISI = linkify(ISI, target = "ISI"),
      DOI = linkify(DOI, target = "DOI"),
      ScopusId = linkify(ScopusId, target = "ScopusID")
    )

}


check_multiplettes_DOI <- function(pubs = kth_diva_pubs()) {

  ScopusId <- Year <- DOI_link <- Scopus_link <- n_pids <- LastUpdated <- NULL

  pubs |>
    select(PID, DOI) |>
    filter(!is.na(DOI)) |>
    count(DOI) |>
    filter(n > 1) |>
    arrange(desc(n)) |>
    inner_join(pubs, by = "DOI") |>
    select(PID, Year, Title, n_pids = n, DOI, ScopusId, LastUpdated, ISI) |>
    mutate(PID = linkify(PID, target = "PID")) |>
    mutate(Title = linkify(Title, target = "titlesearch")) |>
    mutate(DOI = linkify(DOI, target = "DOI")) |>
    mutate(ScopusId = linkify(ScopusId, target = "ScopusId")) |>
    mutate(ISI = linkify(ISI, target = "ISI")) |>
    collect() |>
    select(PID, Year, Title, n_pids, DOI, ISI, ScopusId, LastUpdated) |>
    arrange(desc(n_pids), desc(DOI), desc(LastUpdated))
}

check_multiplettes_scopusid <- function(pubs = kth_diva_pubs()) {

  ScopusId <- n_pids <- Year <- DOI_link <- ScopusId_link <- LastUpdated <-  NULL

  pubs |>
    select(PID, DOI, ScopusId) |>
    filter(!is.na(ScopusId)) |>
    count(ScopusId) |>
    filter(n > 1) |>
    arrange(desc(n)) |>
    inner_join(kth_diva_pubs(), by = "ScopusId") |>
    select(DOI, Year, n_pids = n, PID, Title, ScopusId, ISI, LastUpdated) |>
    mutate(PID = linkify(PID, target = "PID")) |>
    mutate(Title = linkify(Title, target = "titlesearch")) |>
    mutate(DOI = linkify(DOI, target = "DOI")) |>
    mutate(ScopusId = linkify(ScopusId, target = "ScopusId")) |>
    mutate(ISI = linkify(ISI, target = "ISI")) |>
    collect() |>
    select(PID, Year, Title, n_pids, ScopusId, DOI, ISI, LastUpdated) |>
    arrange(desc(n_pids), desc(ScopusId), desc(LastUpdated))
}

check_multiplettes_ISI <- function(pubs = kth_diva_pubs()) {

  # ISI = UT = WoS identifier

  ScopusId <- Year <- n_pids <- n_scopusid <- LastUpdated <- NULL

  pubs |>
  select(PID, ISI, ScopusId) |>
  count(ISI) |>
  filter(n > 1, !is.na(ISI)) |>
  arrange(desc(n)) |>
  inner_join(kth_diva_pubs(), by = "ISI") |>
  select(ISI, Year, n_pids = n, PID, Title, ScopusId, LastUpdated, DOI) |>
  collect() |>
  select(PID, Year, Title, n_pids, ISI, DOI, ScopusId, LastUpdated) |>
  mutate(PID = linkify(PID, target = "PID")) |>
  mutate(ISI = linkify(ISI, target = "ISI")) |>
  mutate(DOI = linkify(DOI, target = "DOI")) |>
  mutate(ScopusId = linkify(ScopusId, target = "ScopusID")) |>
  mutate(Title = linkify(Title, target = "titlesearch")) |>
  arrange(desc(n_pids), desc(ISI), desc(LastUpdated))

}

check_invalid_aid <- function(authors = kth_diva_authors()) {

  # TODO: multiple same kthids in one publication?
  # username in orcid field?

  re_aid <- "^[a-z0-9]{4}$"

  ScopusId <- NULL

  authors |>
    filter(!is.na(kthid)) |>
    filter(!grepl(re_aid, kthid)) |>
    mutate(PID = linkify(PID, target = "PID")) |>
    mutate(ISI = linkify(ISI, target = "ISI")) |>
    mutate(DOI = linkify(DOI, target = "DOI")) |>
    mutate(ScopusId = linkify(ScopusId, target = "ScopusID")) |>
    select(aid = kthid, everything())

}

check_invalid_kthid <- function(authors = kth_diva_authors()) {

  # TODO: multiple same kthids in one publication?
  # username in orcid field?

  re_kthid <- "^u1[a-z0-9]{6}$"
  re_temp <- "^PI\\d+|P\\d+|pi\\d+|p\\d+|Pi\\d+|PI \\d+|-"

  ScopusId <- NULL

  authors |>
    filter(!is.na(kthid)) |>
    filter(!grepl(re_kthid, kthid)) |>
    filter(!grepl(re_temp, kthid)) |>
    mutate(PID = linkify(PID, target = "PID")) |>
    mutate(ISI = linkify(ISI, target = "ISI")) |>
    mutate(DOI = linkify(DOI, target = "DOI")) |>
    mutate(ScopusId = linkify(ScopusId, target = "ScopusID")) |>
    select(-any_of(c("ISRN", "NBN")))

}

check_invalid_orcid <- function(authors = kth_diva_authors(), pubs = kth_diva_pubs(), config = diva_config()) {

  re <- "^([0-9]{4})+(-)+([0-9]{4})+(-)+([0-9]{4})+(-)+([0-9]{3}[0-9Xx]{1})$"

  ScopusId <- Year <- LastUpdated <- NULL

  res <-
    authors |>
    filter(!grepl(re, orcid) & !is.na(orcid)) |>
    left_join(
      pubs |> select(PID, Year, LastUpdated), by = "PID"
    ) |>
    mutate(PID = linkify(PID, target = "PID")) |>
    mutate(ISI = linkify(ISI, target = "ISI")) |>
    mutate(DOI = linkify(DOI, target = "DOI")) |>
    mutate(ScopusId = linkify(ScopusId, target = "ScopusID")) |>
    select(PID, Year, everything()) |>
    select(-any_of(c("ISRN", "NBN")))

  return (res)

  # catch swepub ORCiD checks (for authors not related to divaorg=177)

  # flag_class <- flag_type <- new_value <- old_value <- validation_rule <-
  #   value <- NULL
  #
  # spc <- swepub_checks(config = config)
  #
  # spc |> filter(flag_type == "ORCID", flag_class == "enrichment") |>
  # select(PID, old_value, new_value) |>
  # right_join(by = "PID",
  #   spc |> filter(flag_type == "ORCID", flag_class == "validation") |>
  #     select(PID, validation_rule, orcid = value)
  # ) |>
  # bind_rows(res) |>
  # select_if(Negate(anyNA))

}

check_ambiguous_use_orcid <- function(authors = kth_diva_authors(), pubs = kth_diva_pubs()) {

  autid <- combo <- kthids <- n_kthid <- n_pids <- pidz <- tot_pids <- NULL

  re_orcid <- "^([0-9]{4})+(-)+([0-9]{4})+(-)+([0-9]{4})+(-)+([0-9]{3}[0-9Xx]{1})$"
  re_kthid <- "^u1[a-z0-9]{6}$"

  many <-
    authors |>
    filter(!is.na(orcid)) |>
    select(orcid, kthid, name, autid, PID) |>
    filter(!grepl("^PI", kthid)) |>
    group_by(orcid) |>
    mutate(
      n_kthid = n_distinct(kthid, na.rm = TRUE),
      kthids =  paste(collapse = " ", unique(sort(kthid, na.last = TRUE))),
      tot_pids = n_distinct(PID, na.rm = TRUE)
    ) |>
    group_by(orcid, kthid, name, autid) |>
    mutate(
      n_pids = n_distinct(PID, na.rm = TRUE),
      pids = paste(collapse = " ", unique(PID))
    ) |>
    ungroup() |>
    filter(n_kthid > 1) |>
    select(-c("PID", "n_kthid")) |>
    distinct()

  many |>
    mutate(combo = paste0(linkify(orcid, target = "ORCID"),
      " (tot=", sprintf("%03d", tot_pids), " kthids=", kthids, ")")) |>
    mutate(pidz = strsplit(pids, " ")) |>
    mutate(pidz = pmap_chr(list(pidz), function(x)
      linkify(x, target = "PID") |> head(10) |> paste(collapse = " "))) |>
    select(combo, kthid, name, autid, n_pids, example_pids = pidz) |>
    arrange(combo, kthid, desc(n_pids))

}

check_invalid_scopusid <- function(pubs = kth_diva_pubs()) {

  Year <- NULL
  # from kth-library/bibliutils: r'2-s2\.0-\d{10,11}'
  re <- "^2-s2\\.0-\\d{10,11}$"

  ScopusId <- NULL

  pubs |>
    filter(!is.na(ScopusId)) |>
    filter(!grepl(re, ScopusId)) |>
    mutate(PID = linkify(PID, target = "PID")) |>
    mutate(ISI = linkify(ISI, target = "ISI")) |>
    mutate(DOI = linkify(DOI, target = "DOI")) |>
    mutate(ScopusId = linkify(ScopusId, target = "ScopusID")) |>
    arrange(desc(Year)) |>
    select(Title, Year, ScopusId, PID, ISI, DOI, Year)

}

#' @importFrom libbib check_issn_check_digit
check_invalid_ISSN <- function(pubs = kth_diva_pubs()) {

  # TODO: also check JournalEISSN here too..
  # https://portal.issn.org/api/search?search[]=MUST=allissnbis=%220003-682X%22

  re <- "^\\d{4}-?\\d{3}(X|x|\\d)$"

  has_bad_check <- has_bad_format <- Year <- NULL

  bad_jeissn <-
    pubs |>
    filter(!is.na(JournalEISSN)) |> distinct(JournalEISSN, PID, Year) |>
    mutate(has_bad_check = !libbib::check_issn_check_digit(JournalEISSN, allow.hyphens = TRUE, errors.as.false = TRUE)) |>
    mutate(has_bad_format = !grepl(re, JournalEISSN)) |>
    filter(has_bad_check | has_bad_format)

  bad_jissn <-
    pubs |>
    filter(!is.na(JournalISSN)) |>
    distinct(JournalISSN, PID, Year) |>
    mutate(has_bad_check = !libbib::check_issn_check_digit(JournalISSN, allow.hyphens = TRUE, errors.as.false = TRUE)) |>
    mutate(has_bad_format = !grepl(re, JournalISSN)) |>
    filter(has_bad_check | has_bad_format)

  bind_rows(bad_jeissn, bad_jissn) |>
    select(PID, Year, JournalEISSN, JournalISSN, everything()) |>
    mutate(PID = linkify(PID, target="PID"))

}

#' @import dplyr tidyr purrr
#' @importFrom libbib check_isbn_10_check_digit check_isbn_13_check_digit
check_invalid_ISBN <- function(pubs = kth_diva_pubs()) {

  is_ok_isbn <- function(x) {

    n_digits <- nchar(gsub("-", "", x))

    if (n_digits == 10)
      return(!libbib::check_isbn_10_check_digit(x, allow.hyphens = TRUE, errors.as.false = TRUE))

    if (n_digits == 13)
      return(!libbib::check_isbn_13_check_digit(x, allow.hyphens = TRUE, errors.as.false = TRUE))

    return(FALSE)
  }

  has_bad_check <- has_bad_format <- n_chars <- ISBN <- Year <- NULL

  pubs |>
    filter(!is.na(ISBN)) |> distinct(PID, ISBN, Year) |>
    tidyr::separate_rows(ISBN, sep = ";") |>
    mutate(n_chars = nchar(gsub("-", "", ISBN))) |>
    mutate(has_bad_format = n_chars != 10 & n_chars != 13) |>
    mutate(has_bad_check = map_lgl(ISBN, is_ok_isbn)) |>
    filter(has_bad_check | has_bad_format) |>
    mutate(PID = linkify(PID, target = "PID")) |>
    mutate(ISBN = linkify(ISBN, target = "ISBN"))

}

check_invalid_use_ISBN <- function(pubs = kth_diva_pubs()) {

  Year <- LastUpdated <- ISBN <- PublicationType <- NULL

  pubs |>
    filter(!is.na(ISBN) & Year >= 2020 & PublicationType %in% c(
      "Kapitel i bok, del av antologi",
      "Konferensbidrag",
      "Artikel i tidskrift")) |>
    select(PID, Title, Year, PublicationType, ISBN, LastUpdated) |>
    mutate(
      PID = linkify(PID, target = "PID"),
      Title = linkify(Title, target = "titlesearch"),
      ISBN = linkify(ISBN, target = "ISBN")
    ) |>
    arrange(desc(LastUpdated))
}


check_invalid_authorname <- function(authors = kth_diva_authors()) {

  authors |>
    filter(grepl("^[[:punct:]]{2,}", name)) |>
    select(PID, name, kthid, extorg) |>
    mutate(PID = linkify(PID, target = "PID"))

}

check_published <- function(pubs = kth_diva_pubs()) {

  Year <- is_QCorNQC <- NULL

  t0 <-
    pubs |>
    mutate(Notes = map_chr(Notes, tidy_html))

  t1 <-
    t0 |>
    mutate(has_notes = grepl("QP", Notes)) |>
    mutate(is_not_published = !is.na(Status) &
             (Status %in% c("accepted", "aheadofprint", "inPress"))) |>
    filter(has_notes, is_not_published)

  t2 <-
    t0 |>
    mutate(Notes = map_chr(Notes, tidy_html)) |>
    mutate(is_QCorNQC = grepl("QC|NQC", Notes)) |>
    filter(Status == "submitted",
           is_QCorNQC | PublicationType == "Manuskript (preprint)")

  bind_rows(t1, t2) |>
    select(PID, Year, PublicationType, has_notes,
           is_not_published, Notes, is_QCorNQC)  |>
    collect()
}

check_manuscripts_with_identifiers <- function(pubs = kth_diva_pubs()) {

  # DOIs are usually for published works, sometimes pre-prints provide DOIs though

  # Suppress false positives by excluding DOIs starting with
  # 10.1101 and 10.48550 (bioRxiv and arXiv)
  # https://doi.org/10.1101/2020.08.24.252296
  # https://doi.org/10.48550/ARXIV.2202.11577

  re_exclude <- "^10.1101|^10.48550|^10.2139" # bioRxiv,arXiv and SSRN

  ScopusId <- PMID <- Year <- LastUpdated <- NULL

  pubs |>
    filter(grepl("Manuskript", PublicationType)) |>
    filter(!is.na(DOI) | !is.na(ISI) | !is.na(ScopusId) | !is.na(PMID))  |>
    filter(!grepl(re_exclude, DOI)) |>
    select(PID, Year, Title, DOI, ISI, ScopusId, PMID, LastUpdated) |>
    arrange(desc(LastUpdated)) |>
    mutate(
      Title = linkify(Title, target = "titlesearch"),
      PID = linkify(PID, target = "PID"),
      DOI = linkify(DOI, target = "DOI"),
      ISI = linkify(ISI, target = "ISI"),
      PMID = linkify(PMID, target = "PMID"),
      ScopusId = linkify(ScopusId, target = "ScopusID")
    )

}

#' Data quality checks for DiVA publication data
#'
#' This function makes a number of checks based on publications and author data
#'
#' @return a list with slots for data frames with check results
#' @export
kth_diva_checks <- function() {

  checks <- list(
    article_title_multiplettes = check_multiplettes_article_title |> prefilter_pubs(),
    title_multiplettes = check_multiplettes_title |> prefilter_pubs(),
    submission_status_invalid = check_invalid_submission_status |> prefilter_pubs(),
    missing_kthid = check_missing_kthid |> prefilter_pubs(),
    missing_orcids = check_missing_orcids |> prefilter_pubs(),
    #missing_affiliations = check_missing_affiliations(),
    missing_confpubdate = check_missing_date |> prefilter_pubs(),
    missing_journal_ids = check_missing_journals_identifiers |> prefilter_pubs(),
    odd_book_chapters = check_titles_book_chapters |> prefilter_pubs(),
    invalid_ISI = check_invalid_ISI |> prefilter_pubs(),
    invalid_DOI = check_invalid_DOI |> prefilter_pubs(),
    invalid_ISSN = check_invalid_ISSN |> prefilter_pubs(),
    invalid_kthid = check_invalid_kthid |> prefilter_pubs(),
    invalid_orcid = check_invalid_orcid |> prefilter_pubs(),
    invalid_scopusid = check_invalid_scopusid |> prefilter_pubs(),
    invalid_isbn = check_invalid_ISBN |> prefilter_pubs(),
    invalid_use_isbn = check_invalid_use_ISBN |> prefilter_pubs(),
    invalid_authorname = check_invalid_authorname |> prefilter_pubs(),
    uncertain_published = check_published |> prefilter_pubs(),
    multiplettes_scopusid = check_multiplettes_scopusid |> prefilter_pubs(),
    multiplettes_DOI = check_multiplettes_DOI |> prefilter_pubs(),
    multiplettes_ISI = check_multiplettes_ISI |> prefilter_pubs(),
    manuscripts_with_identifiers = check_manuscripts_with_identifiers |> prefilter_pubs(),
    non_qc = check_non_qc(),
    non_qc_stale = check_non_qc_stale(),
    swepub = swepub_checks(
      year_beg = lubridate::year(Sys.Date()) - 1,
      year_end = lubridate::year(Sys.Date())
    )
  )

  stats <-
    checks |>
    purrr::map(function(x) ifelse(is.null(x), NA, nrow(x))) |>
    tibble::as_tibble() |>
    tidyr::pivot_longer(cols = everything())

  stats[stats$name == "non_qc_stale", "value"] <-
    checks$non_qc_stale |> getElement("n") |> sum()

  checks$stats <- stats

  checks

}

#' Data quality checks for DiVA publication data
#'
#' This function makes a number of checks based on publications and author data
#'
#' @return a list with slots for data frames with check results
#' @param authors a data frame with authors (from diva_download)
#' @param pubs a data frame with pubs (from diva_download)
#' @param config DiVA settings configuration, by default diva_config
#' @export
diva_checks <- function(authors, pubs, config = diva_config()) {

  checks <- list(
    #article_title_multiplettes = check_multiplettes_article_title(pubs),
    title_multiplettes = check_multiplettes_title(pubs),
    submission_status_invalid = check_invalid_submission_status(pubs),
    missing_aid = check_missing_aid(authors, pubs),
    #missing_affiliations = check_missing_affiliations(),
    missing_confpubdate = check_missing_date(pubs),
    missing_journal_ids = check_missing_journals_identifiers(pubs),
    odd_book_chapters = check_titles_book_chapters(pubs),
    invalid_ISI = check_invalid_ISI(pubs),
    invalid_DOI = check_invalid_DOI(pubs),
    invalid_ISSN = check_invalid_ISSN(pubs),
    invalid_aid = check_invalid_aid(authors),
    invalid_orcid = check_invalid_orcid(authors, pubs, config),
#    invalid_scopusid = check_invalid_scopusid(authors),
    invalid_isbn = check_invalid_ISBN(pubs),
    invalid_authorname = check_invalid_authorname(authors),
#    uncertain_published = check_published(pubs),
    multiplettes_scopusid = check_multiplettes_scopusid(pubs),
    multiplettes_DOI = check_multiplettes_DOI(pubs),
    multiplettes_ISI = check_multiplettes_ISI(pubs),
    swepub = swepub_checks(config = config)
  )

  stats <-
    checks |>
    purrr::map(function(x) ifelse(is.null(x), NA, nrow(x))) |>
    tibble::as_tibble() |>
    tidyr::pivot_longer(cols = everything())

  checks$stats <- stats

  checks

}

#' Remaps column names using the internal check_mapping
#' @param x character vector of column names
#' @return character vector of new column names
#' @export
#' @importFrom tibble tibble
#' @importFrom dplyr left_join
#' @importFrom stats setNames
check_remap_colnames <- function(x) {

  desc_swe <- colname_en <- NULL

  colnamez <- kthcorpus::check_mapping

  nm <-
    tibble(colname_en = x) |>
    left_join(colnamez, by = "colname_en") |>
    mutate(desc_swe = ifelse(is.na(desc_swe), colname_en, desc_swe))

  setNames(nm$colname_en, nm$desc_swe)
}

# https://rdrr.io/cran/libbib/man/get_issn_check_digit.html
#library(libbib)
#libbib::get_issn_check_digit()

check_missing_affiliations <- function(authors = kth_diva_authors()) {

  n_aff_ext <- n_aff_kth <- NULL
  # some namestrings have a kthid but no organizational affiliation, examples:
  # parsing PID [1257552] [====>-----------------------------------]  13% eta: 32m
  # No affs in string: Emanuel, Martin [u16f0x93] [0000-0001-6867-5790]
  # parsing PID [1257033] [=======>--------------------------------]  20% eta: 31m
  # No affs in string: Emanuel, Martin [u16f0x93]
  # parsing PID [921548] [=========>-------------------------------]  24% eta: 30m
  # No affs in string: Arlid, Hedda
  # parsing PID [1518530] [===========>----------------------------]  31% eta: 27m
  # No affs in string: Crouzet, Guillemette
  # parsing PID [1556135] [==============>-------------------------]  38% eta: 24m
  # No affs in string: Djurovic, Kristina
  # parsing PID [1257585] [===============>------------------------]  40% eta: 23m
  # No affs in string: Emanuel, Martin [u16f0x93]
  # parsing PID [1257589] [===================>--------------------]  49% eta: 20m
  # No affs in string: Emanuel, Martin [u16f0x93]
  # parsing PID [1449191] [====================>-------------------]  53% eta: 18m
  # No affs in string: Ghauch, Hadi [u1lujhq6] [0000-0002-9442-671X]
  # parsing PID [1349747] [=============================>----------]  75% eta: 10m
  # No affs in string: Bonde, Ingrid
  # parsing PID [1576494] [================================>-------]  82% eta:  7m
  # No affs in string: Almlöf, Jonas [u1f2lz4l] [0000-0002-8721-3580]
  authors |>
    filter(!is.na(kthid) & is.na(n_aff_kth) & is.na(n_aff_ext)) |>
    arrange(desc(n_pid), desc(kthid)) |>
    select(PID, name, kthid, orcid, n_pid)
}

# cma <- check_missing_affiliations()
#
# suggest_affiliation <- function(id)
#   kth_diva_authors() |>
#   filter(kthid == id) |>
#   group_by(orcid, orgids) |>
#   add_count(sort = TRUE) |>
#   arrange(desc(n), desc(orcid)) |>
#   mutate(kthid = id) |>
#   mutate(n_variants = n_groups(.)) |>
#   head(1) |>
#   select(kthid, orcid, orgids, n_variants)
#
# suggest_affiliations <- function(ids)
#   ids |>
#   map_dfr(suggest_affiliation) |>
#   arrange(desc(n_variants))
#
# ids <-
#   cma |>
#   filter(n_pid > 200) |>
#   pull(kthid)
#
# sa <-
#   suggest_affiliations(ids) |>
#   select(suggested_orcid = orcid, suggested_orgid = orgids, everything())
#
# cma |> left_join(sa, by = "kthid")

#' Path to rmarkdown report
#' @export
checks_report_path <- function() {
  system.file("rmarkdown", "checks-report.Rmd", package = "kthcorpus")
}

#' Render rmarkdown report with results from checks
#' @param report path to rmarkdown file, by default checks_report_path()
#' @param use_tmp boolean to indicate if resulting file should be placed in /tmp, default TRUE
#' @export
#' @importFrom rmarkdown render
checks_render_report <- function(report = checks_report_path(), use_tmp = TRUE) {

  # moves bundled rmarkdown files to tempdir if used from shiny
  root <- dirname(normalizePath(report))
  dest <- tempdir()
  on.exit(unlink(dest))

  if (!dir.exists(dest)) dir.create(dest, recursive = TRUE)

  fr <- dir(root, full.names = TRUE)
  to <- file.path(dest, dir(root))

  file.copy(fr, to, overwrite = TRUE)
  src <- file.path(dest, basename(report))
  message("Moving rmarkdown files to temp dir: ", src)

  message("Rendering ", basename(report), "...")
  out <- rmarkdown::render(src, quiet = TRUE)

  if (use_tmp == TRUE) {
    file.copy(out, "/tmp", overwrite = TRUE)
    return(file.path("/tmp", basename(out)))
  }

  return (out)
}

checks_quarto_path <- function(report = "orcid_report.qmd") {
  system.file(package = "kthcorpus", "rmarkdown", report)
}

#' @importFrom quarto quarto_render
checks_render_quarto <- function(report = checks_quarto_path(), use_tmp = TRUE) {

  if (!requireNamespace("quarto", quietly = TRUE)) {
    stop(
      "Package \"quarto\" must be installed to use this function.",
      call. = FALSE
    )
  }

  of <- gsub("\\.qmd", "\\.html", basename(report))
  message("Rendering ", basename(report), " to ", of)

  quarto::quarto_render(
    input = report,
    output_format = "html",
#    execute_dir = "/tmp",
#    output_file = of
  )

  stopifnot(file.exists(of))
  out <- file.path(dirname(report), of)

  if (use_tmp == TRUE) {
    file.copy(out, "/tmp", overwrite = TRUE)
    return(file.path("/tmp", basename(out)))
  }

  return (out)

}

checks_upload_quarto <- function(report = checks_quarto_path(), use_tmp = TRUE) {
  message("Rendering ", report)
  rp <- checks_render_quarto(report, use_tmp = use_tmp)
  message("Uploading ", rp, " to object storage")
  checks_upload_report(rp)
}

#' Upload file to S3
#'
#' @details Checks for minio client being present on the system and assumes
#' an alias "kthb" has been set up, as well as the bucket "kthcorpus"
#' which can be achieved by setting a MC_HOST_kthb environment variable
#' @param path the full path to the file to upload
#' @param dest the path to the destination (s3 alias and bucket), by default "kthb/kthcorpus"
#' @return exit code for upload command (0 means success)
#' @export
checks_upload_report <- function(path, dest = "kthb/kthcorpus") {

  util <- "mc"

  if (startsWith(Sys.getenv("OS"), "Windows"))
    util <- "mc.exe"

  stopifnot(nzchar(Sys.which(util)) || all(file.exists(path)))

  if (Sys.getenv("MC_HOST_kthb") == "" & !file.exists("~/.mc/config.json"))
    warning("Please see if envvar MC_HOST_kthb has been set, or that ~/.mc/config.json exists")

  cmd <- sprintf("%s cp %s %s", util, path, dest)
  res <- system(cmd, timeout = 15)

  if (res == 124) stop("Time out when uploading file ", path)
  if (res != 0) stop("Error when using minio client to upload file, error code: ", res)

  return (res)
}

#' @importFrom stringi stri_escape_unicode
contains_unicode <- function(x) {
  grepl("\\\\u[0-9a-e]{4,}", stringi::stri_escape_unicode(x), perl = TRUE)
}

#' @importFrom stringi stri_escape_unicode stri_extract_all
#' @importFrom purrr map_chr
extract_unicode <- function(x) {
  purrr::map_chr(stringr::str_extract_all(stringi::stri_escape_unicode(x),
    "\\\\u[0-9a-e]{4,}"), .f = function(x)
      ifelse(length(x) == 0 || all(is.na(x)), NA_character_, paste0(x, collapse = " ")))
}

#' @importFrom tibble tibble
#' @importFrom dplyr left_join mutate select
check_identifiers_with_unicode <- function(pub = kth_diva_pubs(), aut = kth_diva_authors()) {

  dataset <- field <- issue <- ua <- NULL

  check_field <- function(data, f) {
    which(contains_unicode(getElement(data, f)))
  }

  report_issue <- function(data, f, label) {
    tibble(rowid = check_field(data, f), field = f, dataset = label) |>
      left_join(data |> mutate(rowid = as.numeric(1:nrow(data))), by = "rowid") |>
      mutate(issue = extract_unicode(getElement(., f))) |>
      select(PID, dataset, field, unicode_issue=issue)
  }


  report_issues <- function(data, fields, label)
    purrr::map_df(fields, function(x) report_issue(data, x, label = label))

  issues <-
    bind_rows(
      report_issues(aut, label = "authors",
        fields = c("kthid", "orcid", "DOI", "ScopusId")),
      report_issues(pub, label = "publications",
        fields = c("JournalISSN", "JournalEISSN", "SeriesISSN", "JournalEISSN", "ISBN", "ISI", "PMID")
      )
    )

  issues |> mutate(PID = linkify(PID, target = "PID"))
}

checks_exclusions_url <- function(csvfile = "exceptions_check_multiplettes_title.csv")
  paste0("https://raw.githubusercontent.com/KTH-Library/kthcorpus/",
    sprintf("main/inst/extdata/%s", csvfile))

checks_exclusions <- function(csvfile = checks_exclusions_url()) {

  res <- csvfile |>
    readr::read_csv(col_types = "dc", locale = readr::locale(encoding = "UTF-8"))

  is_valid <- ncol(res) == 2 & all(names(res) %in% c("PID", "Title"))

  if (!is_valid)
    stop("File ", csvfile, " is either not present or has invalid format")

  return (res)
}

check_invalid_orgid <- function(aut = kth_diva_authors(), pubs = kth_diva_pubs()) {

  orgid <- is_closed <- closed_date <- n_orgs <-
    n_pids <- alt_n_pids <- alt_n_orgs <-
    lc_title <- unit_en <- Year <- py <- cd <-
    overdue_days <- link <- stale_orgids <- alt_orgids <-
    alt_pids <- alt_pidz <- stale_pids <- stale_pidz <- NULL

  diva_orgs <-
    diva_organisations() |>
    mutate(orgid = as.character(orgid))

  orgs_closed <-
    diva_orgs |>
    filter(is_closed) |>
    arrange(desc(closed_date))

  # authors belonging to closed diva organisation ids
  aut_stale <-
    aut |>
    inner_join(orgs_closed, by = "orgid") |>
    arrange(desc(closed_date)) |>
    group_by(name, orcid, kthid, PID, orgid, unit_en) |>
    count() |>
    collect() |>
    ungroup()

  pubs_stale <-
    aut_stale |>
    left_join(pubs |> select(all_of(c("PID", "Title", "PublicationDate", "Year"))), by = c("PID")) |>
    left_join(by = c("orgid", "unit_en"),
      orgs_closed |> mutate(cd = readr::parse_date(as.character(closed_date), trim_ws = TRUE))
    ) |>
    mutate(py = lubridate::ymd(ifelse(!is.na(Year), paste0(Year, "-01-01"), NA))) |>
    mutate(overdue_days = difftime(py, cd, units = "days")) |>
#    select(cd, py, overdue_days) |>
    select(any_of(c("PID", "name", "orcid", "kthid", "Title", "orgid", "unit_en", "py", "cd", "overdue_days"))) |>
    filter(overdue_days > (360 + 180)) |>
    arrange(desc(overdue_days)) |>
    distinct()

  # candidates for non-closed org belongings
  candidate_orgs <-
    aut |>
      anti_join(orgs_closed, by = "orgid") |>
      group_by(name, orcid, kthid, PID, orgid) |>
      count() |>
      collect() |>
      ungroup() |>
      inner_join(
        diva_orgs |>
          filter(!is_closed) |>
          select(is_closed, closed_date, orgid),
        by = "orgid"
      ) |>
      group_by(name, orcid, kthid) |>
      summarise(
        orgids = paste0(collapse = " ", unique(orgid)),
        pids = paste0(collapse = " ", unique(PID)),
        n_orgs = n_distinct(orgid),
        n_pids = n_distinct(PID)
      )

  # authors with stale orgids and their corresponding valid orgids
  recommendations <-
    pubs_stale |>
      group_by(name, orcid, kthid) |>
      summarize(
        stale_pids = paste(collapse = " ", unique(PID)),
        stale_orgids = paste(collapse = " ", unique(orgid))
      ) |>
      left_join(
        candidate_orgs,
        by = c("name", "orcid", "kthid")
      ) |>
      collect() |>
      select(
        everything(),
        alt_orgids = orgids,
        alt_pids = pids,
        alt_n_orgs = n_orgs,
        alt_n_pids = n_pids
      ) |>
      arrange(desc(alt_n_pids), -desc(alt_n_orgs), name)

  # these have one other belonging that is not closed
  # and more than 50 publications
  res <-
    recommendations |>
    filter(alt_n_orgs == 1 & alt_n_pids > 10) |>
    #left_join(pubs_stale |> group_by(kthid) |> summarise(stale_pidz = paste0(collapse = " ", PID)), by = "kthid") |>
    #select(-c("stale_pids", "alt_pids"))
    mutate(stale_pidz = strsplit(stale_pids, " ")) |>
    mutate(stale_pidz = pmap_chr(list(stale_pidz), function(x)
      linkify(x, target = "PID") |> paste(collapse = " "))) |>
    mutate(alt_pidz = strsplit(alt_pids, " ")) |>
    mutate(alt_pidz = pmap_chr(list(alt_pidz), function(x)
      linkify(x, target = "PID") |> paste(collapse = " "))) |>
    select(everything(), -c("stale_pids", "alt_pids"), all_of(c("stale_pidz", "alt_pidz")))


  orgid_to_link <- function(x)
    x |> strsplit(" ") |> #purrr::map(as.integer) |>
    purrr::map(function(x) tibble::tibble(orgid = x) |>
      dplyr::left_join(diva_orgs, by = "orgid")) |>
    purrr::map(function(x) x |>
      dplyr::summarise(link = paste0("<ul>",
        paste0(collapse = "\n", "<li><a href='", url, "' target='_blank'>",
               unit_en, "(", orgid, ", closed:", closed_date, ")", "</a></li>")), "</ul>") |>
        pull(link)
    )

  res |>
    mutate(stale_orgids = orgid_to_link(stale_orgids)) |>
    mutate(alt_orgids = orgid_to_link(alt_orgids)) #|> DT::datatable(escape = F)

}

# TODO: add these checks and refactor existing checks
# (changes needed if also including non-qc publications)

# - all checks should start with ...
#   pubs <-
#      pubs |> filter(grepl("QC ", Notes), CreatedDate < as.Date(Sys.Date() - 30))

# results can be placed under missing/"saknat"
check_non_qc <- function(pubs = kth_diva_pubs()) {

  # one check for which records are not quality checked
  # these publications are recent (from this year) but not marked with QC

  CreatedDate <- LastUpdated <- NULL

  pubs |>
    filter(CreatedDate > as.Date("2023-01-01")) |>
    filter(CreatedDate < as.Date(Sys.Date() - 30), !grepl("QC ", Notes)) |>
    select(PID, CreatedDate, LastUpdated) |>
    mutate(PID = linkify(PID, target = "PID"))
}

check_non_qc_stale <- function(pubs = kth_diva_pubs()) {

  # publications which are not tagged with "QC" but are created more than 30 days ago
  # aggregated by year and publication type

  CreatedDate <- LastUpdated <- y <- NULL

  pubs |>
    filter(CreatedDate < as.Date(Sys.Date() - 30), !grepl("QC ", Notes)) |>
    select(PID, CreatedDate, LastUpdated, PublicationType) |>
    arrange(desc(CreatedDate)) |>
    mutate(PID = linkify(PID, target = "PID")) |>
    group_by(PublicationType, year(CreatedDate)) |>
    count() |>
    ungroup() |>
    rename(y = 2) |>
    arrange(desc(y))
}

