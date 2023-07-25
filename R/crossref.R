read_funder <- function(x) {
  res <- purrr::pluck(x, "message", "items") |>
    tibble::enframe(name = NULL, value = "cr") |>
    tidyr::unnest_wider(simplify = TRUE, col = c("cr"))
  if (!"alt-names" %in% names(res)) return (res)
  res |> tidyr::unnest_wider(col = "alt-names", names_sep = "_")
}

#' Resolve funding organizations from Crossref Funding Registry from a text string
#'
#' The CrossRef Open Funder Registry contains more than 30 000 funder names with DOI URIs
#' @param x a string vector of strings to lookup
#' @return tibble with results
#' @importFrom jsonlite read_json
#' @examples
#' \dontrun{
#' if(interactive()){
#'   cr_funders_resolve(c("rymdstyrelsen", "volvo", "saab"))
#'  }
#' }
#' @export
#' @importFrom tibble enframe
#' @importFrom tidyr unnest_wider
#' @importFrom jsonlite read_json
#' @importFrom utils URLencode
#' @importFrom purrr possibly map map_dfr
#' @import dplyr
cr_funders_resolve <- function(x) {

  y <- x |> unique() |> na.omit()
  param <- utils::URLencode(y, reserved = TRUE)

  urls <- "https://api.crossref.org/funders?query=%s" |>
    sprintf(param)

  reqs <-
    urls |>
    purrr::map(purrr::possibly(jsonlite::read_json), .progress = list(
      show_after = 0.2,
      name = "crossref",
      type = "iterator",
      format = "Fetching {cli::pb_bar} {cli::pb_percent}",
      clear = FALSE
    )) |>
    purrr::map_dfr(read_funder, .id = "seq",
      .progress = list(
        show_after = 0.2,
        name = "crossref",
        type = "iterator",
        format = "Resolving {cli::pb_bar} {cli::pb_percent}",
        clear = FALSE
      )
    )

  res <-
    tibble::tibble(lookup = y, seq = as.character(1:length(y))) |>
    dplyr::left_join(reqs, by = "seq", multiple = "all") |>
    dplyr::select(-any_of("tokens"))

  if (!"id" %in% names(res)) return(data.frame())
  return (res)
}

cr_funders_GET <- function(route = "https://api.crossref.org/funders", filter, rows = 1000, offset = 0, timeout = 30L) {

  # https://api.crossref.org/swagger-ui/index.html#/Funders/get_funders
  ua <- "httr (https://github.com/KTH-Library/kthcorpus/; mailto:biblioteket@kth.se)"

  resp <- httr::GET(url = route,
    query = compact(list(
      filter = filter,
      rows = rows,
      offset = offset
    )),
    httr::add_headers("Content-Type" = "application/json"),
    httr::user_agent(ua),
    httr::timeout(timeout)
  )

  httr::stop_for_status(resp)

  if (httr::http_type(resp) != "application/json") {
    stop(sprintf("API returned %s", httr::http_type(resp)), call. = FALSE)
  }

  if (httr::status_code(resp) != 200) {
    hh <- httr::headers(resp)
    rl_total <- as.integer(hh$`X-Rate-Limit-Limit`)
    rl_interval <- as.integer(hh$`X-Rate-Limit-Interval`)

    stop(
      sprintf(
        "API request failed [%s]\n%s\n<%s>",
        httr::status_code(resp),
        paste0("Rate Limit quota setting:", rl_total),
        paste0("Rate Limit interval: ", rl_interval)
      ),
      call. = FALSE
    )
  }

  return (resp)
}

#' Retrieve Funders from Crossref Funder Registry
#'
#' This function crawls the Crossref Funder Registry for entries in a specific location.
#' @param location the geographical location, Default: 'Sweden'
#' @return tibble with results
#' @examples
#' \dontrun{
#' if(interactive()){
#'  cr_funders()
#'  }
#' }
#' @export
#' @importFrom glue glue
#' @importFrom httr content
#' @importFrom purrr pmap map map_dfr
#' @importFrom dplyr select
cr_funders <- function(location = "Sweden") {

  location <- URLencode(glue::glue("location:{location}"))
  p0 <- cr_funders_GET(filter = location, rows = 1, offset = 0) |> httr::content()

  n <- p0$message$`total-results`
  p <- pager(n, page_length = 100)
  message(glue::glue("Found {n} results, fetchning {nrow(p)} pages..."))

  resp <- list(offset = p$start, rows = p$count) |> purrr::pmap(
    .f = function(rows, offset) cr_funders_GET(filter = location, rows = rows, offset = offset),
    .progress = list(
      show_after = 0.2,
      name = "cr_funders",
      type = "iterator",
      format = "Fetching {cli::pb_bar} {cli::pb_percent}",
      clear = FALSE
    )
  )

  res <- resp |> purrr::map(httr::content) |>
    purrr::map_dfr(read_funder, .id = "seq",
       .progress = list(
         show_after = 0.01,
         name = "crossref",
         type = "iterator",
         format = "Merging {cli::pb_bar} {cli::pb_percent}",
         clear = FALSE
       )
    )

  res |> dplyr::select(-any_of(c("tokens", "seq"))) |>
    dplyr::select(any_of(c("id", "name", "uri")), starts_with("replace")) |>
    unnest_wider(col = "replaced-by", names_sep = "_") |>
    unnest_wider(col = "replaces", names_sep = "_")

}

#' Crossref lookup table for DOI prefixes
#'
#' Requests DOI prefixes from Crossref web site and returns as a data frame.
#' @param doi_prefix the prefix to look up, by default "all"
#' @return data frame
#' @examples
#' \dontrun{
#' if(interactive()){
#'  cr_publisher()  # lookup all publishers
#'  cr_publisher(c("10.4172", "10.4236"))  # lookup given a few doi prefixes
#'  }
#' }
#' @export
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#' @importFrom tidyr unnest
#' @importFrom dplyr arrange rename mutate
#' @importFrom purrr map_df
cr_publisher <- function(doi_prefix = "all") {

  memberId <- prefixes <- publisher <- publisher_name <- NULL

  cr_doi_lookup <- function(url)
    url |> httr::GET()  |> httr::content(as = "text") |>
    jsonlite::fromJSON() |> tibble::as_tibble()

  if (length(doi_prefix) == 1 && doi_prefix == "all") {
    all <-
      paste0("http://doi.crossref.org/getPrefixPublisher/?prefix=", doi_prefix) |>
      cr_doi_lookup() |>
      tidyr::unnest(col = "prefixes") |>
      dplyr::arrange(name) |>
      dplyr::rename(publisher = name, cr_id = memberId, doi_prefix = prefixes) |>
      dplyr::mutate(publisher = html_unescape(publisher))
    return (all)
  }

  # vectorized calls for all give prefixes
  urls <-
    "http://doi.crossref.org/getPrefixPublisher/?prefix=%s" |>
    sprintf(doi_prefix)

  urls |> purrr::map_df(cr_doi_lookup, .progress = TRUE) |>
    dplyr::mutate(publisher_name = html_unescape(publisher_name))
}

html_unescape <- function(x) {
  html <- paste0("<x>", paste0(x, collapse = "#_|"), "</x>")
  parsed <- xml2::xml_text(xml2::read_html(html))
  strsplit(parsed, "#_|", fixed = TRUE)[[1]]
}

#' Crossref assertions associated with a DOI
#'
#' For some DOIs, information like the conference_url can be
#' retrieved.
#' @param doi character the DOI to look up information for
#' @return data frame with assertions data for the DOI
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  # some DOIs do not have assertions...
#'  # "10.1145/263699"  |>
#'  "10.1007/978-3-031-26387-3_24" |>
#'   cr_work_assertions() |>
#'   select(c("doi", starts_with("conference")) |>
#'   glimpse()
#'  }
#' }
#' @export
cr_work_assertions <- function(doi) {
  cr_work_GET(doi) |> parse_cr_assertion(doi)
}

#' @importFrom tibble add_column
parse_cr_assertion <- function(assertion, doi) {
  node <- "assertion"
  assertion |> enframe() |> filter(name == "message") |>
    unnest_wider("value") |>
    select(all_of(node)) |>
    unnest(node) |>
    unnest_wider(node) |>
    select(all_of(c("name", "value"))) |>
    pivot_wider() |>
    add_column(doi = doi, .before = 1)
}

#' @importFrom httr GET content
cr_work_GET <- function(doi) {
  sprintf("https://api.crossref.org/works/%s",
      doi
    ) |>
    httr::GET() |> httr::content()
}

#' Crossref information about conferences, given a DOI
#'
#' Crossref Works API provides information about conference proceedings
#' for some DOIs.
#' @param doi character the DOI to look up information for
#' @return  data frame with conference information
#' @details For more information, see \url{# https://www.crossref.org/documentation/schema-library/markup-guide-record-types/conference-proceedings/}
#' @examples
#' \dontrun{
#' if(interactive()){
#'  cr_work_confinfo("10.1145/263699") |> glimpse()
#'  }
#' }
#' @export
cr_work_confinfo <- function(doi) {
  cr_work_GET(doi) |> parse_cr_confinfo()
}

#' @importFrom dplyr rename_with filter
#' @importFrom tidyr unnest unnest_wider
parse_cr_confinfo <- function(event) {

  node <- "event"

  event |> enframe() |> filter(name == "message") |>
    unnest_wider(col = "value") |>
    select(all_of(node)) |> unnest_wider(col = node) |>
    unnest_wider("start", simplify = list(`date-parts` = FALSE), names_sep = "_") |>
    unnest_wider("end", simplify = list(`date-parts` = FALSE), names_sep = "_") |>
    unnest_wider("start_date-parts", names_sep = "_") |>
    unnest_wider("end_date-parts", names_sep = "_") |>
    unnest_wider("start_date-parts_1", names_sep = "_") |>
    unnest_wider("end_date-parts_1", names_sep = "_") |>
    unnest_wider("sponsor", names_sep = "_") |>
    rename_with(.fn = function(x) gsub("_date-parts_1", "_date", x))
}
