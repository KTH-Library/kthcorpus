wikipedia_countries <- function() {

  alpha_3_code <- country <- country_name <- NULL

  t1 <- "https://en.wikipedia.org/wiki/List_of_ISO_3166_country_codes" |>
    xml2::read_html() |> rvest::html_table() |> getElement(1)

  gs <- function(x, y, z) gsub(pattern = y, replacement = z, x = x)

  colz <-
    t1[1,] |> as.character() |>
    gs("[[].+[]]", "") |> tolower() |>
    gs("\\s+|-", "_")

  t2 <- setNames(t1[-c(1), ], colz)

  t2 |> dplyr::filter(nchar(alpha_3_code) == 3) |>
    dplyr::mutate(country = tolower(alpha_3_code)) |>
    dplyr::select(country, country_name) |>
    dplyr::mutate(country_name = gs(country_name, "[(].*?[)]", "")) |>
    dplyr::mutate(country_name = gs(country_name, "[[].*?[]]", "")) |>
    dplyr::mutate(country_name = stringr::str_trim(country_name))
}

#' ISO3 codes and English country names from Wikipedia
#' @format A data frame with 249 rows and 2 variables:
#' \describe{
#'   \item{\code{country}}{character iso3 character code}
#'   \item{\code{country_name}}{character country name description}
#'}
"countries_iso3"
