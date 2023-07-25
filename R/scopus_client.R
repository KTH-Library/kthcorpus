#' @importFrom lubridate ymd
#' @noMd
#'
format_date <- function(x) {
  ymd(x) %>% remove_char()
}

compact <- function (l) Filter(Negate(is.null), l)

remove_char <- function(x, char = "-") {
  gsub(char, "", x)
}

#' @importFrom httr status_code
#' @noMd
scopus_check_status <- function(res){
  attempt::stop_if_not(
    .x = httr::status_code(res),
    .p = ~ .x == 200,
    msg = paste0("The API returned an error: ", content(res)))
}

#' Configuration for credentials when using Scopus API
#'
#' Use .Renviron to provide credentials for API access.
#'
#' Set values for SCOPUS_API_KEY and optionally for SCOPUS_API_TOKEN if making
#' requests from outside of institutional IPs linked to the subscriber.
#'
#'      file.edit("~/.Renviron")
#'      readRenviron("~/.Renviron")
#' @param quiet whether to display messages for missing environment varibles,
#' by default FALSE
#' @export
scopus_config <- function(quiet = FALSE) {

  key <- Sys.getenv("SCOPUS_API_KEY")
  token <- Sys.getenv("SCOPUS_API_INSTTOKEN")

  if (key == "") {
    if (!quiet) message("No institutional token provided, please use .Renviron to set it in SCOPUS_API_KEY")
    key <- ""
  }

  if (token == "") {
    if (!quiet) message("No institutional token provided, please use .Renviron to set it in SCOPUS_API_INSTTOKEN")
    token <- NULL
  }

  list(
    apiKey = key,
    insttoken = token
  )
}

#' @importFrom httr GET add_headers content
#' @importFrom purrr map_chr possibly pmap map map_df
#' @importFrom dplyr bind_cols
#' @importFrom progress progress_bar
scopus_search_query <- function(criteria) {

  req <- scopus_req(criteria, start = 0, count = 25)

  xml <- httr::content(req)

  hits <- as.integer(xml$`search-results`$`opensearch:totalResults`)
  page_length <- as.integer(xml$`search-results`$`opensearch:itemsPerPage`)
  pages <- purrr::map_chr(xml$`search-results`$link, .f = function(x) as.character(pluck(x, "@href")))
  res <- xml$`search-results`$entry

  if (length(pages) > 1) {
    message("Crawling pages, found ",
      glue::glue("{hits} hits, in {length(pages)} pages, each w at most {page_length} entries"))

    # count pages manually because the search results links appear to be wrong!
    urls <- pages[1:length(pages)]

    cri <- criteria

    mypager <-
      pager(hits, page_length) %>%
      dplyr::bind_cols(tibble(criteria = cri)) %>%
      select(c("criteria", "start", "count"))

    pb <- progress::progress_bar$new(
      format = "  scopus search [:bar] :percent eta: :eta",
      total = nrow(mypager), clear = FALSE, width = 60)

    crawl <- purrr::possibly(
      .f = function(criteria, start, count) {
        pb$tick()
        scopus_crawl(criteria, start, count)
      },
      otherwise = list()
    )

    therest <- mypager %>% purrr::pmap(crawl)

    message("Parsing and flattening nested data...")
    results <- therest %>% purrr::map(parse_scopus_entries)

    merged <- list(
      publications = purrr::map_df(results, "publications") %>% unique(),
      authors = purrr::map_df(results, "authors") %>% unique(),
      affiliations = purrr::map_df(results, "affiliations") %>% unique()
    )

    return(merged)

  }

  return(parse_scopus_entries(res))

}

#' Retrieve publications from Scopus Search API for specified Scopus ids
#'
#' Note: For long vectors of scopus eids, a warning will be issued
#' if the query generates a long query string parameter that may
#' run into issues with recommendations regarding RFC 2616.
#'
#' @param eid character vector of scopus eids, such as "2-s2.0-85144114333"
#' @importFrom glue glue
#' @export
scopus_search_id <- function(eid) {

  if (length(eid) > 1)
    eid <- paste0(collapse = " OR ", eid)

  criteria <- glue::glue(
    "eid({eid})"
  )

  if (nchar(criteria) > 2048)
    warning("Long query string parameter (use < 2048 chars, see RFC 2616)... ")

  scopus_search_query(criteria)

}

#' Retrieve publications from Scopus API from KTH - The Royal Institute of Technology.
#'
#' This function allows for using the "load date" when fetching publications for
#' KTH - The Royal Institute of Technology. By default the time interval for the
#' previous two weeks is used.
#'
#' Note: when using a subscriber API key, requests are only allowed from
#' institutional IPs. From Elsevier's API documentation:
#'
#' "Elsevier Research Products APIs rely primarily on Institutional IP address
#' for authentication. API access through proxies is not supported, however
#' Elsevier will provide remote access direct to the APIs using a special
#' access credential ("Institutional Token").
#' If you are working away from your main institutional network
#' or your institution accesses Scopus.com, Scival.com, or ScienceDirect.com
#' through a proxy, please contact us to enquire about Institutional Token access."
#'
#' The rate limits that apply for using Scopus Search is a max paging length of 25 for
#' complete views with a 5000 item total results limit and weekly 20k results and at
#' the most 9 requests per second.
#'
#' @param beg_loaddate date expressed as "yyyymmdd", by default current date minus 7 days
#' @param end_loaddate date expressed as "yyyymmdd", by default current date
#' @importFrom glue glue
#' @export
scopus_search_pubs_kth <- function(beg_loaddate, end_loaddate) {

  if (missing(end_loaddate))
    end_loaddate <- Sys.Date() %>% format_date()

  if (missing(beg_loaddate))
    beg_loaddate <- (Sys.Date() - 14) %>% format_date()

  beg_pubyear <- 2019L
  id_affiliation <- 60002014L

  criteria <- glue::glue(
    'AFFIL(((kth*) OR (roy* AND inst* AND tech*) OR ("Roy. Inst. T") OR ',
    '(alfven) OR (kung* AND tek* AND hog*) OR (kung* AND tek* AND h\\u00f6g*) OR ',
    '(kgl AND tek* AND hog*) OR (kung* AND tek* AND hg*) OR ',
    '(roy* AND tech* AND univ*)) AND (Sweden)) OR ',
    'AF-ID("The Royal Institute of Technology KTH" {id_affiliation}) AND ',
    'orig-load-date aft {beg_loaddate} AND orig-load-date bef {end_loaddate} AND ',
    'pubyear aft {beg_pubyear} AND NOT PUBSTAGE(AIP)'
  )


  # req <- httr::GET("https://api.elsevier.com/content/search/scopus",
  #   query = list(
  #     query = criteria,
  #     apiKey = scopus_config()$apikey,
  #     view = "COMPLETE"
  #   ),
  #   add_headers("Content-Type" = "application/xml")
  # )

  scopus_search_query(criteria)
}

#' Fetch abstract given Scopus ID
#' @param sid ScopusID
#' @param endpoint the endpoint to use for the request (default value provided)
#' @param timeout the timeout before the request fails, by default 10L
#' @details see https://dev.elsevier.com/documentation/AbstractRetrievalAPI.wadl
#' @examples
#' \dontrun{
#'   scopus_req_abstract("SCOPUS_ID:85140569271")
#' }
scopus_req_abstract <- function(sid,
  endpoint = "https://api.elsevier.com/content/abstract/scopus_id/", timeout = 10L) {

  resp <- httr::GET(
    url = sprintf(paste0(endpoint, "%s"), sid),
    query = compact(list(
      apiKey = scopus_config(quiet=TRUE)$apiKey,
      insttoken = scopus_config(quiet=TRUE)$insttoken,
      view = "FULL" #"FULL"
    )),
    httr::add_headers("Content-Type" = "application/xml"),
    httr::timeout(timeout)
  )

  scopus_check_status(resp)

  if (httr::http_type(resp) != "application/json") {
    stop(sprintf("API returned %s", httr::http_type(resp)), call. = FALSE)
  }

  if (httr::status_code(resp) != 200) {
    hh <- httr::headers(resp)
    rl_total <- as.integer(hh$`x-ratelimit-limit`)
    rl_remaining <- as.integer(hh$`x-ratelimit-remaining`)
    rl_reset_ts <- as.integer(hh$`x-ratelimit-reset`)

    stop(
      sprintf(
        "API request failed [%s]\n%s\n<%s>",
        httr::status_code(resp),
        paste0("Rate Limit quota setting:", rl_total),
        paste0("Remaining RL quota: ", rl_remaining, " which resets at ", rl_reset_ts)
      ),
      call. = FALSE
    )
  }

  return (resp)
}


scopus_req <- function(criteria, start, count) {
  resp <- httr::GET("https://api.elsevier.com/content/search/scopus",
    query = compact(list(
      query = criteria,
      apiKey = scopus_config(quiet=TRUE)$apiKey,
      insttoken = scopus_config(quiet=TRUE)$insttoken,
      view = "COMPLETE",
      sort = "orig-load-date",
      start = start,
      count = count
    )),
    httr::add_headers("Content-Type" = "application/xml"),
    httr::timeout(10L)
  )

  scopus_check_status(resp)

  if (httr::http_type(resp) != "application/json") {
    stop(sprintf("API returned %s", httr::http_type(resp)), call. = FALSE)
  }

  if (httr::status_code(resp) != 200) {
    hh <- httr::headers(resp)
    rl_total <- as.integer(hh$`x-ratelimit-limit`)
    rl_remaining <- as.integer(hh$`x-ratelimit-remaining`)
    rl_reset_ts <- as.integer(hh$`x-ratelimit-reset`)

    stop(
      sprintf(
        "API request failed [%s]\n%s\n<%s>",
        httr::status_code(resp),
        paste0("Rate Limit quota setting:", rl_total),
        paste0("Remaining RL quota: ", rl_remaining, " which resets at ", rl_reset_ts)
      ),
      call. = FALSE
    )
  }

  return (resp)
}

scopus_crawl <- function(criteria, start, count) {
  #res <- httr::content(httr::GET(url, add_headers("Content-Type" = "application/xml")))
  res <- httr::content(scopus_req(criteria, start, count))
  res$`search-results`$entry
}

#' @noRd
#' @importFrom readr read_lines
#' @importFrom purrr map_chr
scopus_fields <- function() {
      I("prism:url
      dc:identifier
      eid
      dc:title
      dc:creator
      prism:publicationName
      prism:issn
      prism:eIssn
      prism:isbn
      prism:volume
      prism:issueIdentifier
      prism:pageRange
      prism:coverDate
      prism:coverDisplayDate
      prism:doi
      dc:description
      citedby-count
      prism:aggregationType
      subtype
      subtypeDescription
      authkeywords
      article-number
      source-id
      fund-no
      openaccess
      openaccessFlag") %>%
    readr::read_lines() %>%
    purrr::map_chr(., .f = function(x) trimws(x))
}

#' @noRd
#' @importFrom readr read_lines
#' @importFrom purrr map_chr map pluck map2_df
#' @importFrom tibble as_tibble tibble
#' @importFrom tidyr unnest_wider
#' @importFrom dplyr select any_of
parse_scopus_entries <- function(xml) {

    afid <- `prism:isbn` <- NULL

    fields <- scopus_fields()

    mylist <-
      purrr::map(fields, function(x)
        purrr::map(xml, x, .default = NA_character_) |> as.character())

    mypubs <-
      tibble::as_tibble(setNames(mylist, nm = fields))

    mypubs <- mypubs |> mutate(`prism:isbn` = `prism:isbn` |>
      map(function(x) eval(parse(text = x))) |>
      map_chr(function(x) map(x, "$") |> unlist() |> paste(collapse = " "))
    )

    t1 <- xml %>% purrr::map(function(x) tibble::tibble(sid = purrr::pluck(x, "dc:identifier")))

    t2 <-
      purrr::map(xml %>% purrr::map(function(x) tibble::tibble(sid = pluck(x, "author"))),
        function(x) tidyr::unnest_wider(x, "sid"))

    myaut <-
      purrr::map2_df(t1, t2, function(x, y) tibble::tibble(x, y)) %>%
      tidyr::unnest(afid) %>% tidyr::unnest(afid) %>% tidyr::unnest(afid) %>%
      dplyr::filter(afid != "true")

    t2 <-
      purrr::map(xml %>% purrr::map(function(x) tibble::tibble(sid = purrr::pluck(x, "affiliation"))),
        function(x) tidyr::unnest_wider(x, "sid"))

    myaff <- purrr::map2_df(t1, t2, function(x, y) tibble::tibble(x, y))

    # extras <- list(
    #   pii = xml %>% map(function(x) find_name(x, "pii")),
    #   article_number = xml %>% map(function(x) find_name(x, "article-number")),
    #   fund_acr = xml %>% map(function(x) find_name(x, "fund-acr"))
    # )

    list(publications = mypubs, authors = myaut, affiliations = myaff)#, object = extras)

}

#' @importFrom dplyr mutate
#' @importFrom tibble tibble
#' @noRd
pager <- function(hits, page_length = 25) {
  n <- hits
  n_pages_full <- ifelse(n %% page_length == 0, n %/% page_length, n %/% page_length + 1)
  from <- seq.int(0, n_pages_full * page_length - 1, by = page_length)
  to <- cumsum(rep(page_length, n_pages_full)) - 1
  to[length(to)] <- n - 1
  tibble(start = from, end = to) %>% dplyr::mutate(count = to - from + 1)
}

#' Authors affiliated with KTH for a default search request
#' @param scopus by default the result from scopus_search_pubs_kth()
#' @importFrom dplyr left_join filter rename
#' @export
scopus_filter_kth_authors <- function(scopus = scopus_search_pubs_kth()) {

  affilname <- `dc:identifier` <- NULL

  scopus$authors %>%
    dplyr::left_join(scopus$affiliations, by = c("afid", "sid")) %>%
    dplyr::filter(stringr::str_starts(affilname, "The Royal Ins")) %>%
    dplyr::left_join(scopus$publications %>%
    dplyr::rename(sid = `dc:identifier`), by = c("sid"))
}

#' Upload Scopus API search results to S3/minio bucket
#' @export
#' @importFrom purrr map2 map
#' @importFrom readr write_csv
scopus_upload <- function() {

  s <- scopus_search_pubs_kth()

  message("Uploading Scopus data to S3 bucket")

  filez <- file.path(tempdir(), sprintf("scopus-%s.csv", names(s)))
    #c("publications", "authors", "affiliations"))
  #)

  on.exit(unlink(filez))

  write <- purrr::map2(s, filez, function(x, y) readr::write_csv(x, file = y))
  upload <- purrr::map(filez, function(x) diva_upload_s3(x))

  return(invisible(TRUE))
}

#' Scopus API ratelimit quota information
#'
#' Makes a request and displays the headers containing rate limit information
#'
#'     X-RateLimit-Limit       <----Shows API quota setting
#'     X-RateLimit-Remaining   <----Shows API remaining quota
#'     X-RateLimit-Reset       1234567891 <----Date/Time in Epoch seconds when API quota resets
#'
#' @return a list with the information from the headers
#' @export
#' @importFrom glue glue
#' @importFrom httr headers status_code
scopus_ratelimit_quota <- function() {

  # make request
  beg_loaddate <- (Sys.Date() - 14) %>% format_date()
  beg_pubyear <- 2019L
  id_affiliation <- 60002014L

  criteria <- glue::glue(
    'AFFIL(((kth*) OR (roy* AND inst* AND tech*) OR ("Roy. Inst. T") OR ',
    '(alfven) OR (kung* AND tek* AND hog*) OR (kung* AND tek* AND h\\u00f6g*) OR ',
    '(kgl AND tek* AND hog*) OR (kung* AND tek* AND hg*) OR ',
    '(roy* AND tech* AND univ*)) AND (Sweden)) OR ',
    'AF-ID("The Royal Institute of Technology KTH" {id_affiliation}) AND ',
    'orig-load-date aft {beg_loaddate} AND pubyear aft {beg_pubyear}'
  )

  resp <- scopus_req(criteria, 1, 1)

  # Read the RateLimit headers

  hh <- httr::headers(resp)
  rl_total <- as.integer(hh$`x-ratelimit-limit`)
  rl_remaining <- as.integer(hh$`x-ratelimit-remaining`)
  rl_reset_ts <- as.integer(hh$`x-ratelimit-reset`)

  msg <-
    sprintf(
      "API rate limit info: [%s]\n%s\n%s",
      paste0("API request HTTP status: ", httr::status_code(resp)),
      paste0("Rate Limit quota setting: ", rl_total),
      paste0("Remaining RL quota: ", rl_remaining, " which resets at ", rl_reset_ts)
    )

  message(msg)

  utc_reset <- as.POSIXct(rl_reset_ts, tz = "UTC", origin = "1970-01-01")
  utc_now <- as.POSIXct(Sys.time(), tz = "UTC")

  message("Reset in UTC is: ", utc_reset)
  message("  Now in UTC is: ", utc_now)

  list(
    `X-RateLimit-Limit` = rl_total,
    `X-RateLimit-Remaining` = rl_remaining,
    `X-RateLimit-Reset` = rl_reset_ts
  )
}


pluck_raw_org <- function(x) {

  org <-
    x |> rget("organization", parents = "affiliation", new_name = "org") |>
    pull(1) |> na.omit() |> paste(collapse = ", ")

  ce_source <-
    x |> rget("ce:source-text", parents = "affiliation", new_name = "ce_source") |>
    pull(1) |> na.omit() |> paste(collapse = ", ")

  ce_text <-
    x |> rget("ce:text", parents = "affiliation", new_name = "ce_text") |>
    pull(1) |> na.omit() |> paste(collapse = ", ")

  raw <-
    x |> rget("$", parents = "affiliation", new_name = "raw") |>
    pull(1) |> na.omit() |> paste(collapse = ", ")

  ap <-
    x |> rget("address-part", parents = "affiliation", new_name = "ap") |>
    pull(1) |> na.omit() |> paste(collapse = ", ")

  tibble(org = org, ce_source = ce_source, ce_text = ce_text, raw = raw, ap = ap) |>
    mutate(across(where(is.character), .fns = function(x) na_if(x, ""))) |>
    mutate(raw = ifelse(nzchar(ce_source) && nchar(ce_source) > nchar(raw), NA, raw)) |>
#    mutate(ce_source = ifelse(nzchar(raw) && nchar(raw) < nchar(ce_source), NA, ce_source)) |>
    mutate(raw_org = paste0(collapse = ", ", na.omit(c(org, ce_source, raw, ap, ce_text)))) |>
    mutate(across(where(is.character), .fns = function(x) na_if(x, "")))
}

#' Request extended information from Scopus Abstract API
#'
#' The response includes the abstract itself, associated author groups with
#'    data including source text for affiliated organisations, and affiliated
#' @param sid ScopusID such as "SCOPUS_ID:85140569271" or "85140569271"
#' @return list with three slots for abstract, authorgroups and correspondence
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  mysid <- scopus$publications$`dc:identifier`[1]
#'  scopus_abstract_extended(mysid)
#'  }
#' }
#' @seealso
#'  \code{\link[httr]{content}}
#'  \code{\link[readr]{parse_atomic}}
#' @export
#' @importFrom httr content
#' @importFrom readr parse_integer
#' @importFrom purrr map_chr map_int pluck
#' @importFrom dplyr bind_cols tibble
scopus_abstract_extended <- function(sid) {

  # clean identifiers (if prefigated, ie long scopus ids or eids)
  is_eid <- any(grepl("^2-s2.0-", sid))
  is_long_sid <- any(grepl("^SCOPUS_ID:", sid))

  if (is_eid | is_long_sid)
    sid <- gsub("(2-s2.0-)|(SCOPUS_ID:)", "", sid)

  abstract <- scopus_req_abstract(sid = sid, timeout = 20L) |> httr::content()

  text <- abstract$`abstracts-retrieval-response`$coredata |>
    pluck(.default = NA_character_, "dc:description")

  # pluck_org <- function(x) {
  #
  #   ce_source <-
  #     pluck(x, .default = NA_character_,
  #       "affiliation", "ce:source-text")
  #
  #   if (!all(is.na(ce_source))) {
  #     return(ce_source |> paste(collapse = ", "))
  #   }
  #
  #   org <- pluck(x, .default = NA_character_,
  #     "affiliation", "organization")
  #
  #   if (!all(is.na(org))) org |> map_chr("$", .default = NULL) |>
  #     paste0(collapse = ", ")
  #
  # }
  #
  # pluck_org2 <- function(x) {
  #
  #   if (x |> pluck_exists("affiliation", "ce:source-text")) {
  #     ce_source <- x |> pluck("affiliation", "ce:source-text") |>
  #       paste0(collapse = ", ")
  #     return (ce_source)
  #   }
  #
  #   if (x |> pluck_exists("affiliation", "address-part")) {
  #     ap <- x|> pluck("affiliation", "address-part") |>
  #       paste0(collapse = ", ")
  #   }
  #
  #   if (x |> pluck_exists("affiliation", "organization", "$")) {
  #     org <- x|> pluck("affiliation", "organization", "$")
  #   }
  #
  #   paste0(collapse = ", ", c(org, ap))
  # }
  #
  #
  # pluck_aut <- function(x) {
  #
  #   pluck(x, .default = NA_character_,
  #     "author", 1
  #   )
  #
  # }

  ag <-
    abstract$`abstracts-retrieval-response`$item$bibrecord$head$`author-group`

  # has_unwrapped_key <- function(key)
  #   all(names(map(ag, key)) %in% c("affiliation", "author"))

  needs_wrap <- function(obj, key) {
    unname(map(obj, key)) %>% map_lgl(is.null) %>% all()
  }

  # is_unwrapped_author <-
  #   has_unwrapped_key("author") && unname(lengths(ag)["author"]) == 1
  #
  # is_unwrapped_aff <-
  #   has_unwrapped_key("affiliation") && unname(lengths(ag)["affiliation"]) == 1

  if (needs_wrap(ag, "affiliation") | needs_wrap(ag, "author"))
    ag <- list(ag)

  raw_org <-
    ag |> map_dfr(pluck_raw_org, .id = "id")
#    ag |> map(pluck_org2) |> map_dfr(.f = function(x) tibble(raw_org = x), .id = "id")

  value <- NULL

  # fields_aut <-  strsplit(split = "\\s+",
  #   "preferred-name @seq ce:initials @_fa @type ce:surname @auid @orcid ce:indexed-name") |>
  #   unlist()
  #
  # aut <-
  #   fields_aut |> map(function(x) rget(ag, parents = "author", x)) |> unlist() |> bind_rows()

  aut <-
    map(ag, "author") |> compact() |> map_dfr(tibble::enframe, .id = "id")  |>
#    slice(1) |> pull(value) |> unlist() |> bind_rows()
    mutate(value = map(value, function(x) bind_rows(unlist(x)))) |>
    unnest(value) |>
#    pull(value) |> bind_rows()
#    mutate(value = map(value, function(x) bind_cols(x, .name_repair = "minimal")))
#    rowwise() |>
#    mutate(value = list(as.data.frame(value))) |>
#    ungroup() |>
#    pmap_dfr(.f = function(id, name, value) tibble(id, name) %>% bind_cols(value)) |>
#    rename_with(function(x) chartr(".X", "_x", x)) |>
#    rename_with(function(x) gsub("__", "_", fixed = TRUE, x = x)) |>
#    rename_with(function(x) gsub("x_", "", fixed = TRUE, x = x)) |>
    rename_with(function(x) chartr(".:-", "___", x)) |>
    rename_with(function(x) gsub("@", "", fixed = TRUE, x = x)) |>
    rename_with(function(x) gsub("_fa", "fa", fixed = TRUE, x = x)) |>
    #colnames()
    select(id, i = name, everything())

  aff <-
    map(ag, "affiliation") |>  map(tibble::enframe) |>
    map(function(x) x |> rowwise() |>
      mutate(value = paste0(collapse = " ", unique(unlist(value))))) |>
    map(function(x) mutate(x, name = gsub("@", "x_", name))) |>
    map(function(x) pivot_wider(x, names_from = "name", values_from = "value")) |>
    map_dfr(bind_rows, .id = "id") |>
    rename_with(function(x) chartr("-:", "__", x)) |>
    rename_with(function(x) gsub("x_country", "country3", fixed = TRUE, x = x)) |>
    rename_with(function(x) gsub("x_", "", fixed = TRUE, x = x)) |>
    left_join(raw_org, by = "id")

  authorgroup <-
    aut  |>  left_join(aff, by = "id") |>
    dplyr::bind_cols(sid = sid) |>
    select(sid, everything())

  # relevant with respect to Article Processing Charges / APC costs
  cor <-
    abstract$`abstracts-retrieval-response`$item$bibrecord$head$correspondence

  is_single_cor <- cor |> pluck("person") |> names() |> grepl(pattern = "^ce") |> all()

  if (needs_wrap(cor, "person"))
    cor <- list(cor)

  correspondence <-
    cor |> map(pluck("person")) |> map_dfr(as_tibble) |> # %>% pluck("person")
    rename_with(function(x) chartr("-:", "__", x)) |>
    dplyr::bind_cols(sid = sid) |>
    select(sid, everything())

  a <- abstract

  fields <-
    strsplit(split = "\\s+",
      "dc:publisher srctype prism:coverDate prism:aggregationType source-id
      citedby-count prism:volume subtype openaccess prism:issn prism:eIssn
      prism:issueIdentifier subtypeDescription prism:publicationName openaccessFlag
      prism:doi prism:startingPage prism:endingPage dc:identifier") |>
    unlist()

  coredata <-
    fields |> map(function(x) rget(a, x)) |> bind_cols() #unlist() |> bind_rows()

  # lookup publisher from crossref if not available from scopus
  # if (is.na(coredata$`dc:publisher`)) {
  #   doi <- coredata$`prism:doi`
  #   if (nzchar(doi)) {
  #     #dois <-
  #     #scopus$publications |> filter(`dc:identifier` %in% sids) |>
  #       #select("prism:doi") |> pull(1)
  #     cr_publisher <-
  #       rcrossref::cr_works(dois = doi)$data$publisher
  #     coredata$`dc:publisher` <- cr_publisher
  #   }
  # }

  lang <- a |> find_name("language")
  coredata$lang <- ifelse(is.null(lang), "eng", lang |> as.character())

  keywords <-
    a |> rget("$", parents = "author-keyword", new_name = "keywords") |>
    pull("keywords") |> paste0(collapse = ", ")

  if (keywords == "NA")
    keywords <- NA_character_

  coredata$keywords <- keywords

  #a <- "85147745927" |> scopus_req_abstract() |> httr::content()
  # coredata <- list(
  #   a |> rget("dc:publisher"),
  #   a |> rget("srctype"), # "j" (coredata)
  #   a |> rget("prism:coverDate"), # "2023-01-20"
  #   a |> rget("prism:aggregationType"), # "Journal
  #   a |> rget("source-id"),  # 25593
  #   a |> rget("citedby-count"), # "0"
  #   a |> rget("prism:volume"), # "62"
  #   a |> rget("subtype"), # ar
  #   a |> rget("openaccess"), # "0"
  #   a |> rget("prism:issn"), #: "21553165 1559128X",
  #   a |> rget("prism:issueIdentifier"), # "3"
  #   a |> rget("subtypeDescription"), # "Article"
  #   a |> rget("prism:publicationName"), #: "Applied Optics",
  #   a |> rget("openaccessFlag"), #: "false",
  #   a |> rget("prism:doi"), #: "10.1364/AO.478405",
  #   a |> rget("prism:startingPage"), #: "541",
  #   a |> rget("dc:identifier"), #: "SCOPUS_ID:85147731960",
  #   a |> rget("@xml:lang", parents = "language"), # : {"@xml:lang": "eng"},
  #   a |> rget("authkeywords") #: null
  # )

  ci <- data.frame()

  if (abstract$`abstracts-retrieval-response`$coredata$subtype == "cp") {
    beg <- coredata$"prism:startingPage"
    end <- coredata$"prism:endingPage"
    ci <- parse_confinfo(abstract) |>
      tibble::add_column(conf_ext_start = beg, conf_ext_end = end)
  }

  list(
    scopus_abstract = coredata |> bind_cols(
      sid = sid,
      `dc:description` = tidy_xml(text)
    ),
    scopus_authors = aut,
    scopus_authorgroup = authorgroup,
    scopus_correspondence = correspondence,
    scopus_confinfo = ci
  )
}

parse_confinfo <- function(abstract) {

  date_beg <- date_end <- beg <- end <-
    conf_details <- conf_sourcetitle <- conf_issuetitle <-
    date_beg_day <- date_beg_month <- date_beg_year <-
    date_end_day <- date_end_month <- date_end_year <-
    y <- m <- d <- Var1 <- Var2 <- var <- is_populated <- NULL

  # sids <- paste0("SCOPUS_ID:85162277054 SCOPUS_ID:85162208085 ",
  #   "SCOPUS_ID:85162205964 SCOPUS_ID:85162194454 SCOPUS_ID:85162223284 ",
  #   "SCOPUS_ID:85162210747 SCOPUS_ID:85162197463 SCOPUS_ID:85162224833 ",
  #   "SCOPUS_ID:85162265813 SCOPUS_ID:85162219211 SCOPUS_ID:85162253865 ",
  #   "SCOPUS_ID:85162196928 SCOPUS_ID:85162258109 SCOPUS_ID:85162256230 ",
  #   "SCOPUS_ID:85162191207 SCOPUS_ID:85162217474 SCOPUS_ID:85162268691 ",
  #   "SCOPUS_ID:85162208893 SCOPUS_ID:85162201437 SCOPUS_ID:85162197216 ",
  #   "SCOPUS_ID:85162005310 SCOPUS_ID:85162006176 SCOPUS_ID:85161974029 ",
  #   "SCOPUS_ID:85161999337 SCOPUS_ID:85161990555 SCOPUS_ID:85161889600 ",
  #   "SCOPUS_ID:85161931704 SCOPUS_ID:85161853142 SCOPUS_ID:85161836541") |>
  #   strsplit(split = "\\s+") |> unlist() |> gsub(pattern = "SCOPUS_ID:", replacement = "")
  #
  # sid <- sids[1]
  # abstract <- scopus_req_abstract(sid = sid) |> httr::content()

  source <-
    #eid |> scopus_req_abstract() |>  httr::content() |>
    abstract$`abstracts-retrieval-response`$item$bibrecord$head$source

  event <- source$`additional-srcinfo`$conferenceinfo$confevent

  aci <- list(
    conf_title = event$confname,
    city = event$conflocation$city,
    country = event$conflocation$`@country`,
    date_end = event$confdate$enddate,
    date_beg = event$confdate$startdate,
    conf_sourcetitle = source$sourcetitle,
    conf_seriestitle = event$confseriestitle,
    conf_issuetitle = source$issuetitle
  )

  wc <- kthcorpus::countries_iso3

  my_aci <-
    list(aci) |>
    tibble::enframe() |>
    tidyr::unnest_wider(col = "value") |>
    tidyr::unnest_wider(col = date_beg, names_sep = "") |>
    tidyr::unnest_wider(col = date_end, names_sep = "") |>
    dplyr::rename_with(.fn = function(x) gsub("@", "_", x), dplyr::starts_with("date")) |>
    left_join(wc, by = "country")

  has_range <-
    sprintf("date_%s", c("beg", "end")) |>
    expand.grid(c("year", "month", "day")) |>
    mutate(var = paste(Var1, Var2, sep = "_")) |>
    mutate(is_populated = var %in% names(my_aci)) |>
    select(var, is_populated)

  has_pattern <- function(re)
    has_range |> filter(is_populated) |> pull(var) |>
    grepl(pattern = re) |> all()

  has_end_date <- has_pattern("_end_")
  has_beg_date <- has_pattern("_beg_")

  if (all(has_range$is_populated)) {
    res <- my_aci |>
    mutate(
      beg = lubridate::make_date(date_beg_year, date_beg_month, date_beg_day),
      end = lubridate::make_date(date_end_year, date_end_month, date_end_day),
      beg_my = paste(months(beg), year(beg)),
      end_my = paste(months(end), year(end)),
      dur = purrr::pmap_chr(
        .l = list(end, beg),
        .f = function(x, y) {
          #x <- ifelse(x != "NA", x, NA)
          na.omit(c(x, y)) |> format("%b %-d %Y") |> paste(collapse = " - ")
        }
      )
    ) |>
    mutate(conf_details = glue::glue("{conf_title}, {city}, {country_name}, {dur}")) |>
    select(conf_title = conf_issuetitle, conf_details) |>
    mutate(conf_subtitle = NA)
    return(res)
  } else if (has_end_date) {
    res <-
      my_aci |> rename(
        y = date_end_year,
        m = date_end_month,
        d = date_end_day
      )
  } else if (has_beg_date) {
    res <-
      my_aci |> rename(
        y = date_beg_year,
        m = date_beg_month,
        d = date_beg_day
      )
  } else {
    stop("Cannot find date range or complete date for conference")
  }

  res |>
    mutate(
      end = lubridate::make_date(y, m, d),
      dur = end |> format("%b %-d %Y") |> glue::glue(na = "")
    ) |>
    mutate(conf_details = glue::glue("{conf_title}, {city}, {country_name}, {dur}")) |>
    select(conf_title = conf_issuetitle, conf_details) |>
    mutate(conf_subtitle = NA)
}

tidy_xml <- function(x, cdata = FALSE) {

  if (cdata) {
    res <- glue::glue("<![CDATA[{x}]]>", .na = "")
    return (res)
  }

  chars <- tibble::tribble(
    ~from, ~to,
    "\"", "&quot;",
    "'", "&apos;",
    "<", "&lt;",
    ">", "&gt;",
    "&", "&amp;"
  )

  stringi::stri_replace_all_fixed(x,
    pattern = chars$from,
    replacement = chars$to,
    vectorize_all = F
  )

}

#' Classify subject categories given a Scopus identifier
#' @param sid a scopus identifier
#' @param scopus by default publication given by scopus_from_minio()
#' @return a tibble with results from SwePubs classification API
scopus_classify <- function(sid, scopus = scopus_from_minio()) {

  `dc:description`<- `dc:identifier` <- `dc:title`<-
    authkeywords <- NULL

  my_abstract <-
    scopus_abstract_extended(sid)$scopus_abstract |>
    pull(`dc:description`)

  my_title <-
    scopus$publications |>
    filter(grepl(sid, `dc:identifier`)) |>
    pull(`dc:title`)

  my_keywords <-
    scopus$publications |>
    filter(grepl(sid, `dc:identifier`)) |>
    pull(authkeywords) |>
    gsub(pattern = " [|] ", replacement = ", ")

  #my_classes <- NULL

  classify_swepub(my_title, my_abstract, my_keywords, level = "5")
}

#' Browse a Scopus electronic identifier
#' @param eid Electronic Identifier
#' @importFrom utils browseURL
#' @export
scopus_browse <- function(eid) {

  id <- ifelse(grepl("SCOPUS_ID:", eid), gsub("SCOPUS_ID:", "2-s2.0-", eid), eid)
  utils::browseURL(sprintf(
    "http://www.scopus.com/record/display.url?origin=inward&partnerID=40&eid=%s",
    id))
}

find_name <- function(haystack, needle) {
  hasName <- NULL
  if (hasName(haystack, needle)) {
    haystack[[needle]]
  } else if (is.list(haystack)) {
    for (obj in haystack) {
      ret <- Recall(obj, needle)
      if (!is.null(ret)) return(ret)
    }
  } else {
   NULL
  }
}

rget <- function(x, field, siblings = NULL, parents = NULL, new_name = field) {

  condition_n <- function(x, .xname) .xname == field

  condition_s <- function(x, .xname, .xsiblings)
    .xname == field & siblings %in% .xsiblings

  condition_p <- function(x, .xname, .xparents)
    .xname == field & parents %in% .xparents

  my_condition <- condition_n

  if (!is.null(siblings)) {
    my_condition <- condition_s
  }

  if (!is.null(parents)) {
    my_condition <- condition_p
  }

  my_accessor <- function(x, .xsiblings, .xparents, .xpos) {
    #      print("siblings are: ", as.character(.xsiblings))
    res <- x
    return (x)
  }

  res <- x |> rrapply::rrapply(
    condition = my_condition,
    f = my_accessor,
    how = "flatten",
    options = list(namecols = TRUE)  # TODO: change this?
  )

  #if (length(res) == 1) return(res) # unname(res))
  ret <- tibble(x = res) #|> setNames(nm = new_name)
  if (nrow(ret) < 1) ret <- tibble(x = NA_character_)
  colnames(ret) <- new_name
  return (ret)
}

