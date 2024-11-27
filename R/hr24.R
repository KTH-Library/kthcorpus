#' Fetch latest HR file
#'
#' @param bucket The bucket to look in for the file, default hr24
#' @param filedate an optional date to look for, default Sys.Date
#' @import stringr lubridate aws.s3 dplyr
#' @importFrom utils read.csv
#' @export
fetch_hr24 <- function(bucket = 'hr24', filedate = Sys.Date()) {

  lastname <- firstname <- email <- NULL

  if(!is.Date(filedate))
    stop("Please use a filedate of format YYYY-MM-DD")

  files <- get_bucket_df(bucket) |>
    filter(grepl('20[0-9]{2}-[0-1][0-9]-[0-3][0-9]', Key)) |>
    mutate(date = gsub('.*(20[0-9]{2}-[0-1][0-9]-[0-3][0-9]).*', '\\1', Key))

  filename <- files |>
    filter(date == filedate) |>
    pull(Key)

  if(length(filename) != 1) {
    stop("No or more than one HR file(s) with date ", filedate, " in ", bucket)
  }

  message("Reading ", filename, " from ", bucket)

  hr <- s3read_using(utils::read.csv, sep = ";", fileEncoding = "UTF-8-BOM", object = filename, bucket = bucket)


  cols <- data.frame(
    oldname = c("KTHID", "F\u00d6DELSE\u00c5R", "ORG_NR", "ORG_NAMN", "EFTERNAMN", "F\u00d6RNAMN", "KTH_EMAIL", "MAN.KVINNA",
                "TJ_BEN_KOD", "TJ_BEN_TEXT", "BEF_NR", "BEF_FROM", "DATUM_NUV_BEF", "BEF_TOM", "SYSS_GRAD",
                "\u00c4MNESKOD", "FUNKTION_SV", "SKOLA"),
    newname = c("kthid", "yob", "unit_abbr", "unit_name", "lastname", "firstname", "email", "gender",
                "emp_code", "emp_desc", "emp_nr", "emp_first_beg", "emp_beg", "emp_end", "emp_degree",
                "scb_topic", "emp_title_swe", "school_name"))

  namediff <- setdiff(names(hr), cols$oldname)
  if(length(namediff) > 1)
    warning("There are unknown new fields in HR24 data: ", paste(namediff, collapse = ", "))

  school_abbr <- data.frame(unit_school = c("ABE", "CBH", "EECS", "ITM", "SCI", "VS"),
                            school_name = c("Skolan f\u00f6r arkitektur och samh\u00e4llsbyggnad",
                                            "Skolan f\u00f6r kemi, biologi och h\u00e4lsa",
                                            "Skolan f\u00f6r elektronik och datavetenskap",
                                            "Skolan f\u00f6r industriell teknik och management",
                                            "Skolan f\u00f6r teknikvetenskap",
                                            "Verksamhetsst\u00f6d"))

  hr |>
    rename_with(.fn = \(x) cols |> filter(oldname == x) |> pull(newname), .cols = cols$oldname) |>
    left_join(school_abbr, by = "school_name") |>
    mutate(emp_beg = as_date(emp_beg),
           emp_first_beg = as_date(emp_first_beg),
           emp_end = if_else(emp_end == '', as_date('2999-12-31'), as_date(emp_end)),
           lastname = trimws(str_remove(lastname, "D\u00F6dsbo")),
           firstname = trimws(firstname),
           fullname = paste0(lastname,", ", firstname),
           plainname = paste(firstname, lastname),
           username = gsub("@.*$", "", email))
}
