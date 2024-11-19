oai_url <- function() {
  "https://kth.diva-portal.org/dice/oai"
}

oai_db_path <- function() {
  file.path(rappdirs::app_dir("kthcorpus")$config(), "oai.db") |>
  normalizePath()
}

oai_con <- function() {
  d <- oai_db_path() |> suppressWarnings()
  if (!file.exists(d)) {
    message("Cannot find ", d, " ... attempting download")
    if (!dir.exists(dirname(d))) 
      dir.create(dirname(d))
    diva_download_s3(files = "oai.db", destination = d)
    stopifnot(file.exists(d))
  }
  con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = d)
  DBI::dbExecute(con, "SET autoinstall_known_extensions=1;")
  DBI::dbExecute(con, "SET autoload_known_extensions=1;")
  return (con)
}

#' @importFrom oai list_metadataformats list_sets
oai_hello <- function(url = oai_url()) {
  service <- url |> oai::id() |> tibble::as_tibble()
  meta <- url |> oai::list_metadataformats() |> tibble::as_tibble()
  sets <- url |> oai::list_sets() |> tibble::as_tibble()
  list(service = service, meta = meta, sets = sets)
}

oai_db_rm <- function() {
  unlink(oai_db_path())
}

#' @importFrom oai list_identifiers
oai_identifiers_kth <- function(since) {
  message("... patience please ... 5 - 10 minutes ...")
  tictoc::tic()
  #ids <- oai_url() |> list_identifiers(from = "2010-01-01T", set = "all-kth")
  if (missing(since)) {
    ids <- oai_url() |> oai::list_identifiers(set = "all-kth")
  } else {
    ids <- oai_url() |> oai::list_identifiers(from = since, set = "all-kth")
  }
  tictoc::toc()

  return(ids)

}

oai_record <- function(ids, oai = oai_url(), 
  format = oai_hello(url = oai)$meta$metadataPrefix) {

  mf <- match.arg(format)

  ids |>
    oai::get_records(prefix = mf, url = oai, as = "raw") |>
    tibble::enframe(name = "identifier", value = "mods") |>
    tidyr::unnest("mods")
}

oai_crawl_records <- function(ids, batch_size = 100L) {

  pages <- split(ids, ceiling(seq_along(1:length(ids)) / batch_size))

  pb <- progress::progress_bar$new(
    format = "(:spin) harvesting :percent (:current) :elapsed [:bar] [:eta]",
    total = length(pages), width = 63
  )

  oai_crawl <- function(x) {
    pb$tick()
    oai_record(x, format = "swepub_mods")
  }

  crawl_or_proceed <- purrr::possibly(oai_crawl, otherwise = NULL)

  pages |>
    purrr::map_df(crawl_or_proceed, .progress = TRUE)

}

oai_db_lastmod <- function(con) {

  datestamp <- ts <- NULL

  if (missing(con)) {
    con <- oai_con()
    on.exit({
      DBI::dbDisconnect(con, shutdown = TRUE)
    })
  }

  stopifnot(DBI::dbExistsTable(con, "ids"))

  ids <- 
    con |> dplyr::tbl("ids") |> collect() |> 
    readr::type_convert() |> suppressMessages()

  ids |> dplyr::summarise(ts = max(datestamp)) |> 
    pull(ts) |> strftime("%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  
}

#' @importFrom dbplyr dbplyr_edition
oai_changes <- function(con, append = FALSE, since = oai_db_lastmod()) {

  if (missing(con)) {
    con <- oai_con()
    on.exit({
      DBI::dbDisconnect(con, shutdown = TRUE)
    })
  }

  edition <- dbplyr::dbplyr_edition()
  message("Using dbplyr edition ", edition)

  lastmod <- since
  message("Fetching changes since last db datestamp: ", lastmod)
  ids <- oai_identifiers_kth(since = lastmod) |> 
    readr::type_convert() |> suppressMessages()

  ids_old <- 
    con |> dplyr::tbl("ids") |> dplyr::collect() |> 
    readr::type_convert() |> suppressMessages()

  ids_diff <- dplyr::anti_join(ids, ids_old)

  new_ids <- 
    ids_diff |> 
    dplyr::group_by(identifier) |> 
    dplyr::filter(datestamp == max(datestamp)) |> 
    dplyr::ungroup()

  print(new_ids)

  if (isTRUE(append)) {
    message("Appending new identifiers to ids table")
    res <- con |> DBI::dbWriteTable("ids", new_ids, append = TRUE)
  }
  
  message("Fetching MODS for new identifiers")

  tictoc::tic()
  new_mods <- new_ids$identifier |> oai_crawl_records()
  tictoc::toc()

  if (isTRUE(append)) {
    message("Appending records to mods table")
    res <- con |> DBI::dbWriteTable("mods", new_mods, append = TRUE)
  }

  tictoc::tic()
  new_extra <- oai_db_enrich(con, new_mods)
  tictoc::toc()

  if (isTRUE(append)) {
    message("Appending mods in xml and json to mods_extra table")
    res <- con |> DBI::dbWriteTable("mods_extra", new_extra, append = TRUE)
  }

  out <- list(
    ids = new_ids,
    mods = new_mods,
    extra = new_extra
  )

  return(out)
  
}

oai_record_status <- function(mods_xml_string, quiet = TRUE) {
  x <- mods_xml_string
  myxml <- xml2::read_xml(x)
  header <- xml2::xml_find_all(xml2::xml_ns_strip(myxml), "//GetRecord//header")
  res <- header |> xml2::xml_attr("status")
  if (length(res) > 1) {
    if (!quiet) message("Found more than one status value in xml: \n", as.character(header))
    return(res |> unique() |> na.omit() %>% paste(collapse = " "))
  }
  return(res)
}

xfind <- function(xml, xpath) {
  x <- read_xml(xml)
  res <- xml_find_all(xml_ns_strip(x), xpath) |> xml_text()
  if (length(res) < 1) return(NA_character_)
  if (length(res) > 1) return(paste(collapse = " ", res))
  return(res)
}

oai_mods_statuses <- function(con, records) {

  mods <- identifier <- status <- NULL

  if (missing(con)) {
    con <- oai_con()
    on.exit({
      DBI::dbDisconnect(con, shutdown = TRUE)
    })
  }

  if (missing(records)) {
    records <- con |> tbl("mods") |> collect()  
  }
  
  records |> 
    mutate(status = purrr::map_chr(mods, oai_record_status, .progress = TRUE)) |>
    select(identifier, mods, status)
}

oai_mods_pids <- function(con, records) {

  mods <- NULL

  if (missing(con)) {
    con <- oai_con()
    on.exit({
      DBI::dbDisconnect(con, shutdown = TRUE)
    })
  }

  if (missing(records)) {
    records <- con |> tbl("mods") |> collect()  
  }

  records |>
    mutate(PID = purrr::map_chr(mods, function(x) xfind(x, "//recordIdentifier"), .progress = TRUE)) |>
    filter(!is.na(PID)) |> 
    mutate(PID = gsub("diva2:", "", PID)) |>
    mutate(PID_many = grepl(" ", PID))   
}

oai_mods_jsons <- function(con, records) {

  if (missing(con)) {
    con <- oai_con()
    on.exit({
      DBI::dbDisconnect(con, shutdown = TRUE)
    })
  }

  mods <- NULL

  if (missing(records)) {
    records <- con |> tbl("mods") |> collect()  
  }

  records |> 
    mutate(json = purrr::map_chr(mods, \(x) possibly(mods_to_json, NA_character_)(x), .progress = TRUE))
  
}

oai_db_enrich <- function(con, records) {

  if (missing(con)) {
    con <- oai_con()
    on.exit({
      DBI::dbDisconnect(con, shutdown = TRUE)
    })
  }

  if (missing(records)) {
    records <- con |> tbl("mods") |> collect()  
  }

  t1 <- records |> oai_mods_jsons(con = con)
  t2 <- records |> oai_mods_statuses(con = con)
  t3 <- records |> oai_mods_pids(con = con)  
  t4 <- t1 |> left_join(t2) |> left_join(t3)

  records |> left_join(t4) |> 
    select(any_of(c("PID", "PID_many", "status")), everything()) |> 
    select(!any_of(c("mods")))

}

oai_db_s3_upload <- function() {
  options("minioclient.dir" = dirname(Sys.which("mc")))
  db <- oai_db_path()
  minioclient::mc_cp(db, "kthb/kthcorpus/oai.db")
}

oai_db_s3_download <- function() {
  options("minioclient.dir" = dirname(Sys.which("mc")))
  db <- oai_db_path()
  minioclient::mc_cp("kthb/kthcorpus/oai.db", db)
}

oai_db_init <- function() {
  if (!file.exists(oai_db_path())) {
    message("No database exists locally, downloading...")
    oai_db_s3_download()
  }
}

oai_db_upsert <- function(con, table_name, data) {

  if (missing(con)) {
    con <- oai_con()
    #con |> DBI::dbExecute(statement = "BEGIN TRANSACTION;")
    on.exit({
      #DBI::dbExecute(con, statement = "COMMIT;CHECKPOINT;")
      DBI::dbDisconnect(con, shutdown = TRUE)
    })
  }

  is_staged <- 
    sprintf("create or replace temp table staging as 
      select * from %s where 1 = 2;", table_name) |> 
      DBI::dbExecute(conn = con) == 0
  
  has_index <- 
    sprintf("create unique index if not exists idx_%s on %s(identifier);",
      table_name, table_name) |> 
    DBI::dbExecute(conn = con) == 0

  # is_deduplicated <-     
  # "create table mods_extra2 as from mods_extra qualify row_number() over 
  # (partition by identifier) = 1" |> 
  #   DBI::dbExecute(conn = con)
  # "drop table mods_extra" |> DBI::dbExecute(conn = con)
  # "create table mods_extra as from mods_extra2" |> DBI::dbExecute(conn = con)
  # "from mods_extra limit 5" |> DBI::dbGetQuery(conn = con) |> as_tibble()

  is_appended <- 
    DBI::dbWriteTable(conn = con, 
      name = "staging", value = data, append = TRUE) == TRUE

  # "delete from staging" |> DBI::dbExecute(conn = con)
  #"from staging" |> DBI::dbGetQuery(conn = con) |> as_tibble()

  #"from ids select * where identifier = 'oai:DiVA.org:kth-354364'" |> DBI::dbGetQuery(conn = con)
  
  n_merged <- 
    sprintf("insert or replace into %s by name 
      select distinct(*) from staging order by identifier;", table_name) |> 
    DBI::dbExecute(conn = con)

  is_dropped <- 
    "drop table staging;" |> 
    DBI::dbExecute(conn = con) == 0
  
  return(n_merged)
}

duckdb_s3_query <- function(query) {

  s3_x <- 
    system("mc alias --json ls kthb", intern = TRUE) |> 
    jsonlite::fromJSON()
  
  s3_setup_auth <- 
    "create secret minio_secret (
      TYPE S3, USE_SSL true, URL_STYLE path,
      KEY_ID '%s', SECRET '%s', ENDPOINT '%s');" |> 
      sprintf(s3_x$accessKey, s3_x$secretKey, s3_x$URL |> 
        gsub(pattern="https://", replacement=""))
  
  con <- duckdb::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  con |> DBI::dbExecute(
    'SET autoinstall_known_extensions=1;
    SET autoload_known_extensions=1;')
  
  con |> DBI::dbExecute(s3_setup_auth)  
  con |> DBI::dbGetQuery(query) |> tibble::as_tibble()

}

oai_db_refresh <- function(con, since_ts) {

  if (missing(con)) {
    con <- oai_con()
    con |> DBI::dbExecute(statement = "BEGIN TRANSACTION;")
    on.exit({
      DBI::dbExecute(con, statement = "COMMIT;CHECKPOINT;")
      DBI::dbDisconnect(con, shutdown = TRUE)
    })
  }

  if (missing(since_ts)) {
    news <- con |> oai_changes(append = FALSE)
  } else {
    news <- con |> oai_changes(append = FALSE, since = since_ts)
  }

  n_ids <- con |> oai_db_upsert(table_name = "ids", data = news$ids)
  n_mods <- con |> oai_db_upsert(table_name = "mods", data = news$mods)
  n_extra <- con |> oai_db_upsert(table_name = "mods_extra", data = news$extra)

  list(n_ids = n_ids, n_mods = n_mods, n_extra = n_extra)

}
