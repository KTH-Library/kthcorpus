#' List contents of minio S3 bucket
#'
#' Before using this function, set up credentials for accessing minio using S3
#'
#' @details
#' For example use environment variables like these:
#'   #AWS_ACCESS_KEY_ID=supersecret
#'   #AWS_SECRET_ACCESS_KEY=supersecret
#'   #AWS_S3_ENDPOINT=lib.kth.se
#'   #AWS_DEFAULT_REGION=data
#' This can be achieved by adding these lines to the ~/.Renviron file.
#'   #file.edit("~/.Renviron")
#'   #readRenviron("~/.Renviron")
#' @param bucket name of bucket to list
#' @importFrom aws.s3 get_bucket_df
#' @importFrom dplyr arrange
#' @export
hr_ls <- function(bucket = "hrplus") {

  # these are available buckets/datasets
  #bucket_list_df(use_https = FALSE)

  # these are the files in the hrplus bucket
  aws.s3::get_bucket_df(bucket, use_https = uses_https(), max = Inf) |>
    arrange(desc(LastModified))
}

read_minio <- function(bucket = "hrplus", file, offset) {

  my_files <- hr_ls() |> pull("Key")
  my_file <- my_files |> head(1)

  if (!missing(file)) {
    stopifnot(file %in% my_files)
    my_file <- file
  }

  if (!missing(offset)) {
    stopifnot(as.integer(offset) == offset)
    ts <- stringr::str_extract(my_file, "\\d{8}") # Get date of latest file
    ts2 <- format(lubridate::parse_date_time(ts, "%Y%m%d") - lubridate::days(offset), "%Y%m%d")
    my_file <- my_files[grepl(ts2, my_files)] |> head(1)
    stopifnot(length(my_file) == 1)
  }

  f1 <- get_object(my_file, bucket, use_https = uses_https())

}

#' Parse content from HR-plus data export
#' @param bucket name of bucket to list
#' @param file optional character string for file name in bucket
#' @param offset optional offset in days (when retrieving older files)
#' @importFrom `aws.s3` get_object
#' @importFrom utils head capture.output
#' @export
hr_plus <- function(bucket = "hrplus", file, offset) {
  f1 <- read_minio(bucket, file, offset)
  hr_read_csv(f1)
}

#' Parse and read the HR data in CSV format
#' @param file path to file
#' @importFrom readr read_csv cols parse_double locale parse_integer problems
#' @importFrom dplyr rename_with mutate contains everything
#' @importFrom purrr map_dfr
#' @importFrom utils capture.output
#' @import lubridate
#' @export
hr_read_csv <- function(file) {

  cs <- cols(.default = col_character())

  # parse and remap colnames; use lowersnakecase field names
  # to fix R pkg warn: esc <- function(x) cat(stringi::stri_escape_unicode(x))

  mapping <-
    kthcorpus::hr_mapping # %>% filter(!(export %in% c("FÖRNAMN", "ORG_NR")))

  hr_map <- function(x) {
    tibble(export = x) %>%
      inner_join(mapping, by = "export") %>%
      pull("colname")
  }

  # fix issues with added fields not present in mapping, ie
  # "Names must be unique. Names are duplicated.
  #  In names[cols] <- .fn(names[cols], ...) :
  #  number of items to replace is not a multiple of replacement length"

  hr <- read_csv(file = file, col_types = cs, quote = "\"")

  cn <- colnames(hr)

  if (length(cn) > nrow(mapping)) {
    mismatch <- setdiff(colnames(hr), mapping$export)
    warning("New fields have been added, not present in hr_mapping: ",
            paste0(collapse = ", ", mismatch))
    hr <- hr %>% select(-any_of(mismatch))
  }

  hr <-
    hr %>%
    rename_with(.fn = hr_map, .cols = colnames(hr))

  if (nrow(problems(hr)) > 0) {
    #warning("Inconsistent data format in csv file: ", file)
    out <- capture.output(problems(hr))
    message("Inconsistent data format in csv file (is header complete?): ")
    print(out)
  }

  # data types parsing

  intcols <- c("yob", "emp_nr")
  dtecols <- c("emp_beg", "emp_lastmod", "emp_end")

  hr %>%
    mutate(across(.cols = contains(dtecols), .fns = ymd)) %>%
    mutate(emp_degree = parse_double(emp_degree, locale = locale(decimal_mark = ","))) %>%
    mutate(across(.cols = contains(intcols), .fns = parse_integer))

  typed <-
    hr %>%
    mutate(across(.cols = contains(dtecols), .fns = ymd)) %>%
    mutate(emp_degree = parse_double(emp_degree, locale = locale(decimal_mark = ","))) %>%
    mutate(across(.cols = contains(intcols), .fns = parse_integer))

  probs <- map_dfr(typed %>% select(kthid, all_of(c(intcols, dtecols))), problems)

  if (nrow(probs) > 0) {
    info <- hr %>% select(kthid, all_of(c(intcols, dtecols))) %>%
      slice(probs$row) %>% mutate(row = probs$row) %>%
      inner_join(probs, by = "row") %>%
      select(kthid, row, everything())
    w <- paste0(collapse = "\n", capture.output(info))
    warning("Proceeding, but with parsing issues for row(s): ",
      paste(sep = ", ", probs$row), "\n\n", w, "\n")
    w2 <- read_lines(file)[probs$row + 1]
    warning("Raw data:\n\n", w2)
  }

  return(typed)

}
