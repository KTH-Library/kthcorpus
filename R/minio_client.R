uses_https <- function() {
  is_https <- TRUE
  if (Sys.getenv("AWS_DEFAULT_PROTOCOL") == "http") is_https <- FALSE
  is_https
}

#' List files in an S3 bucket
#' @param bucket the bucket to use, default is "bibliometrics"
#' @importFrom aws.s3 get_bucket_df
#' @importFrom dplyr arrange
#' @export
minio_ls <- function(bucket = "bibliometrics") {
  get_bucket_df(bucket, use_https = uses_https()) %>%
    arrange(desc(LastModified))

}

#' Get a file from an S3 bucket
#' @param file the file
#' @param bucket the bucket to use, default is "bibliometrics"
#' @importFrom aws.s3 get_object
#' @importFrom dplyr filter pull
#' @export
minio_get <- function(file, bucket = "bibliometrics") {
  my_file <- minio_ls(bucket = bucket) %>% filter(Key == file) %>% pull("Key")
  get_object(my_file, bucket, use_https = uses_https())
}

#' @importFrom minioclient mc_ls
#' @importFrom stringr str_extract
#' @importFrom lubridate parse_date_time
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate
#' @importFrom purrr map_dbl
#' @importFrom readr read_lines
mc_ls_tbl <- function(...) {

  lines <- minioclient::mc_ls(...) |> suppressMessages()
  lines <- lines$stdout |> readr::read_lines()

  re_time <- "\\[(.*?)\\s+(.*?)\\s+(.*?)\\]"
  re_size <- "\\]\\s+(\\d+\\.*\\d*)(.*?B)"
  re_all <- "\\[(.*?)\\s+(.*?)\\s+(.*?)\\]\\s+(\\d+\\.*\\d*)(.*B)\\s+(.*)$"
  re_fn <- "((.*?)\\s+)*(.*)$"

  dt <- stringr::str_extract(lines, re_time, 1)
  ts <- stringr::str_extract(lines, re_time, 2)
  tz <- stringr::str_extract(lines, re_time, 3)
  sz <- stringr::str_extract(lines, re_size, 1) |> as.double()
  sz_unit <- stringr::str_extract(lines, re_size, 2)
  ending <- stringr::str_extract(lines, re_all, 6)
  fn <- stringr::str_extract(ending, re_fn, 3)
  att <- stringr::str_extract(ending, re_fn, 2)

#  dt <- lubridate::parse_date_time(paste(dt, ts), tz = unique(tz)[1], orders = "Ymd HMS")

  data.frame(dt = dt, sz = sz, sz_unit = sz_unit, type = att, fn = fn) |>
    tibble::as_tibble() |>
    mutate(size = paste(sz, sz_unit) |> purrr::map_dbl(convert_to_bytes)) |>
    mutate(dt = purrr::map2_vec(paste(dt, ts), tz, .f = purrr::possibly(
      .f = function(x, y) lubridate::parse_date_time(x, orders = "Ymd HMS", tz = y),
      otherwise = NA_Date_
    )))
}

#mc_ls_tbl("kthb/kthcorpus")

convert_to_bytes <- function(value) {
  #utils:::format.object_size(53e4, units = "MiB", standard = "IEC")
  #utils:::object.size()

  digits <- gsub("[^0-9.]", "", value)
  units <- gsub("[0-9.]", "", value) |> trimws()

  multiplier <- switch (units,
    'GiB' = 1024 ^ 3,
    'MiB' = 1024 ^ 2,
    'KiB'= 1024 ^ 1,
    'B' = 1024 ^0)

  as.double(digits) * multiplier
}

mc_read <- function(fn) {
  tf <- tempfile()
  on.exit(unlink(tf))
  minioclient::mc_cp(from = fn, to = tf)
  stopifnot(file.exists(tf))
  readr::read_file_raw(tf)
}

