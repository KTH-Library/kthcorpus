#' @noRd
#' @importFrom readr write_file
download_searchdb <- function() {
  # TODO: make independent of platform OS
  sdb <- minio_get("search.db", bucket = "kthcorpus")
  readr::write_file(sdb, file.path(rappdirs::app_dir("kthcorpus")$config(), "search.db"))
}

#' @noRd
#' @importFrom DBI dbConnect
#' @importFrom RSQLite SQLite
con_search <- function(db_loc = file.path(rappdirs::app_dir("kthcorpus")$config(), "search.db")) {
  if (!file.exists(db_loc)) download_searchdb()
  if (!file.exists(db_loc)) stop("Failed downloading search db... Network error?")
  DBI::dbConnect(RSQLite::SQLite(), db_loc)
}

# for a set of search terms we generate the sql for an FTS search query
# and make sure to include the score
# NB: quirky column order
fts_query <- function(search_term, colz) {
  terms <- paste0(collapse = " ", search_term)
  paste(
    sprintf("select bm25(fts) as score, %s from fts", colz),
    sprintf("where fts match 'NEAR(%s)' order by bm25(fts);", terms))
}

# fcn to execute a search against the database and return a tibble
match_near <- function(search_term, con = con_search(), colz, lookup) {
  score <- NULL
  terms <- strsplit(split = "\\s+", search_term) |> unlist()
  term <- paste0(collapse = " ", paste0("\"", terms, "\""))
  con |> DBI::dbGetQuery(fts_query(term, colz)) |> as_tibble() |>
    mutate(score = abs(score)) |>
    mutate(search = search_term) |>
    mutate(term = search_term) |>
    select(term, any_of(colnames(lookup)), score) |>
    mutate(across(-c("score"), as.character)) |>
    mutate(across("score", as.double))
}

# fcn to quickly "tokenize" a sentence and pick out the
# first few words to use in the search
extract_n_words <- function(x, n = 3) {
  re <- paste0("^((?:\\S+\\s+){", n, "}\\S+).*")
  sub(re, "\\1", x, perl = TRUE) |> tolower() |>
    gsub(pattern = "[^[:alnum:] ]|and", replacement = "")
}

# for a set of project names, this fcn looks for 0..* matches
search_names <- function(term) {

  rowid <- score <- NULL

  con <- con_search(db_loc = file.path(rappdirs::app_dir("kthcorpus")$config(), "search.db"))
  on.exit(DBI::dbDisconnect(con))

  colz <- con |> DBI::dbListFields("fts") |> paste0(collapse = ", ")
  lookup <- con |> DBI::dbReadTable("lookup")

  lookups <-
    term |>
    purrr::map_chr(function(x) extract_n_words(x, n = 4), .progress = TRUE) |>
    purrr::map_dfr(function(x) match_near(x, con, colz, lookup), .id = "rowid", .progress = TRUE)

  tibble::tibble(term = term) |>
    tibble::rowid_to_column() |>
    mutate(rowid = as.character(rowid)) |>
    left_join(lookups, by = "rowid") |>
    arrange(desc(score))
}
