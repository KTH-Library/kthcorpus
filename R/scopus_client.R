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
#' @importFrom httr GET add_headers content
#' @importFrom glue glue
#' @importFrom purrr map_chr possibly pmap map map_df
#' @importFrom dplyr bind_cols
#' @importFrom progress progress_bar
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
    'orig-load-date aft {beg_loaddate} AND pubyear aft {beg_pubyear}'
  )

  # req <- httr::GET("https://api.elsevier.com/content/search/scopus",
  #   query = list(
  #     query = criteria,
  #     apiKey = scopus_config()$apikey,
  #     view = "COMPLETE"
  #   ),
  #   add_headers("Content-Type" = "application/xml")
  # )

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

#' Fetch abstract given Scopus ID
#' @param sid ScopusID
#' @param endpoint the endpoint to use for the request (default value provided)
#' @details see https://dev.elsevier.com/documentation/AbstractRetrievalAPI.wadl
#' @examples
#' \dontrun{
#'   scopus_req_abstract("SCOPUS_ID:85140569271")
#' }
scopus_req_abstract <- function(sid,
  endpoint = "https://api.elsevier.com/content/abstract/scopus_id/") {

  resp <- httr::GET(
    url = sprintf(paste0(endpoint, "%s"), sid),
    query = compact(list(
      apiKey = scopus_config(quiet=TRUE)$apiKey,
      insttoken = scopus_config(quiet=TRUE)$insttoken,
      view = "FULL" #"FULL"
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

    afid <- NULL

    fields <- scopus_fields()

    mylist <-
      purrr::map(fields, function(x)
        purrr::map(xml, x, .default = NA_character_) |> as.character())

    mypubs <-
      tibble::as_tibble(setNames(mylist, nm = fields))

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
    dplyr::left_join(ss$affiliations, by = c("afid", "sid")) %>%
    dplyr::filter(stringr::str_starts(affilname, "The Royal Ins")) %>%
    dplyr::left_join(ss$publications %>%
    dplyr::rename(sid = `dc:identifier`), by = c("sid"))
}

#' Upload Scopus API search results to S3/minio bucket
#' @export
#' @importFrom purrr map2 map
#' @importFrom readr write_csv
scopus_upload <- function() {

  s <- scopus_search_pubs_kth()

  message("Uploading Scopus data to S3 bucket")

  filez <- file.path(tempdir(), sprintf("scopus-%s.csv",
    c("publications", "affiliations", "authors"))
  )

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

  #sid <- sidz[1]

  abstract <- scopus_req_abstract(sid = sid) |> httr::content()

  text <-
    abstract$`abstracts-retrieval-response`$coredata$`dc:description`

  pluck_org <- function(x) {

    ce_source <-
      pluck(x, .default = NA_character_,
        "affiliation", "ce:source-text")

    if (!is.na(ce_source)) {
      return(ce_source |> paste(collapse = ";"))
    }

    org <- pluck(x, .default = NA_character_,
      "affiliation", "organization")

    if (!is.na(org)) org |> map_chr("$") |>
      paste0(collapse = "; ")
  }

  pluck_aut <- function(x) {

    pluck(x, .default = NA_character_,
      "author", 1
    )

  }

  ag <-
    abstract$`abstracts-retrieval-response`$item$bibrecord$head$`author-group`

  has_unwrapped_key <- function(key)
    all(names(map(ag, key)) %in% c("affiliation", "author"))

  needs_wrap <- function(obj, key) {
    unname(map(obj, key)) %>% map_lgl(is.null) %>% all()
  }

  is_unwrapped_author <-
    has_unwrapped_key("author") && unname(lengths(ag)["author"]) == 1

  is_unwrapped_aff <-
    has_unwrapped_key("affiliation") && unname(lengths(ag)["affiliation"]) == 1

  if (needs_wrap(ag, "affiliation") | needs_wrap(ag, "author"))
    ag <- list(ag)

  raw_org <-
    ag |> map(pluck_org) |> map_dfr(.f = function(x) tibble(raw_org = x), .id = "id")

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
    map(ag, "affiliation") %>% map(tibble::enframe) |>
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
    aut %>% left_join(aff, by = "id") |>
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
      citedby-count prism:volume subtype openaccess prism:issn
      prism:issueIdentifier subtypeDescription prism:publicationName openaccessFlag
      prism:doi prism:startingPage dc:identifier") |>
    unlist()

  coredata <-
    fields |> map(function(x) rget(a, x)) |> unlist() |> bind_rows()

  coredata$lang <- a |> find_name("language") |> as.character()

  keywords <-
    a |> rget("$", parents = "author-keyword", new_name = "keywords") |>
    pull("keywords") |> paste0(collapse = ", ")

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

  list(
    scopus_abstract = coredata |> bind_cols(
      sid = sid,
      `dc:description` = tidy_xml(text)
    ),
    scopus_authorgroup = authorgroup,
    scopus_correspondence = correspondence #,
  )
}

tidy_xml <- function(x, cdata = FALSE) {

  if (cdata)
    return (sprintf("<![CDATA[%s]]>", x))

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

  my_condition <- function(x, .xname) .xname == field

  if (!is.null(siblings)) {
    my_condition <- function(x, .xname, .xsiblings)
      .xname == field & siblings %in% .xsiblings
  }

  if (!is.null(parents)) {
    my_condition <- function(x, .xname, .xparents)
      .xname == field & parents %in% .xparents
  }

  my_accessor <- function(x, .xsiblings, .xparents, .xpos) {
    #      print("siblings are: ", as.character(.xsiblings))
    return(x)
  }

  res <- x |> rrapply::rrapply(
    condition = my_condition,
    f = my_accessor,
    how = "flatten",
    options = list(namecols = TRUE)
  )

  if (length(res) == 1) return(res) # unname(res))
  tibble(x = res) |> setNames(nm = new_name)
}