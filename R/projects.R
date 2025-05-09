#' @import dplyr cordis
kth_cordis <- function(use_refresh = FALSE) {
  #devtools::install_github("kth-library/cordis")

  if (!requireNamespace("cordis", quietly = TRUE)) {
    stop(
      "Package \"cordis\" must be installed to use this function.",
      call. = FALSE
    )
  }

  shortName <- projectID <- projectAcronym <- totalCost <- NULL

  cordis::cordis_import(refresh = use_refresh)

  #cordis:::cordis_dropdb(confirm = TRUE)
  #cordis::cordis_import()

  con <- cordis::cordis_con()
  on.exit(duckdb::dbDisconnect(con))

  he_project_ids <-
    con |> tbl("he_organization") |>
    filter(shortName == "KTH") |> collect() |>
    distinct(projectID, projectAcronym) |>
    pull(projectID)

  he_projects <-
    con |> tbl("he_project") |> filter(id %in% he_project_ids) |> collect()

  #he_projects |> View()

  fp7_project_ids <-
    con |> tbl("fp7_organization") |>
    filter(shortName == "KTH") |> collect() |>
    distinct(projectID, projectAcronym) |>
    pull(projectID)

  fp7_projects <-
    con |> tbl("fp7_project") |> filter(id %in% fp7_project_ids) |> collect()

  if (is.character(fp7_projects$totalCost)) {
    fp7_projects$totalCost <- readr::parse_number(fp7_projects$totalCost)
  }

  h2020_project_ids <-
    con |> tbl("h2020_organization") |>
    filter(shortName == "KTH") |> collect() |>
    distinct(projectID, projectAcronym) |>
    pull(projectID)

  h2020_projects <-
    con |> tbl("h2020_project") |> filter(id %in% h2020_project_ids) |> collect()

  # combine Horizon Europe, H2020 and FP7 projects into one table
  kth_cordis <-
    list(h2020_projects, he_projects, fp7_projects) |> bind_rows()

  return (kth_cordis)
  #cordis::cordis_tables()

}

#' @import formas
kth_formas <- function() {

  if (!requireNamespace("formas", quietly = TRUE)) {
    stop(
      "Package \"formas\" must be installed to use this function.",
      call. = FALSE
    )
  }

  tictoc::tic()

  message("Requesting projects from Formas")
  formas <- formas::formas_projects()

  #message("Requesting Formas lookup tables")
  #flt <- formas::formas_lookup_tables()

  # extract those related to KTH
  formas_rows <-
    formas |> getElement("medelsf\u00f6rvaltareNamn") |>
    grep(pattern = "Kungl.* Tekniska h.gskolan")

  kth_formas <- formas[formas_rows,]

  tictoc::toc()
  message("Done")

  return (kth_formas)

}

#' @import vinnova
kth_vinnova <- function() {

  if (!requireNamespace("vinnova", quietly = TRUE)) {
    stop(
      "Package \"vinnova\" must be installed to use this function.",
      call. = FALSE
    )
  }

  message("Requesting projects from Vinnova")

  tictoc::tic()
  vinnova <- vinnova::vinnova_latest(from_date = "2010-01-01")

  vinnova_ids <-
    vinnova$projects$KoordinatorOrg |>
    grep(pattern = "Kungliga") |> unique()

  kth_vinnova <- vinnova$projects[vinnova_ids,]
  tictoc::toc()

  message("Done")

  return(kth_vinnova)

}

#' @import cordis
cordis_swe_vat <- function() {

  if (!requireNamespace("cordis", quietly = TRUE)) {
    stop(
      "Package \"cordis\" must be installed to use this function.",
      call. = FALSE
    )
  }

  organisationID <- vatNumber <- shortName <- name <- country <- NULL

  con <- cordis::cordis_con()
  on.exit(duckdb::dbDisconnect(con, shutdown = TRUE))

  cordis_org_vat <-
    list("he_organization", "fp7_organization", "h2020_organization") |>
    purrr::map(
      function(x) con |> tbl(x) |>
        filter(country == "SE") |>
        distinct(organisationID, vatNumber, shortName, name) |>
        collect()
    ) |> bind_rows() |> distinct() |> select(-shortName) |>
    filter(!is.na(vatNumber))

  return(cordis_org_vat)
}

#' @import swecris
kth_swecris <- function() {

  if (!requireNamespace("swecris", quietly = TRUE)) {
    stop(
      "Package \"swecris\" must be installed to use this function.",
      call. = FALSE
    )
  }

  organisationNameSv <- organisationId <- projectId <- peopleList <-
    scbs <- NULL

  message("Requesting projects from SweCRIS")

  tictoc::tic()

  kth_orgid <-
    swecris::swecris_organisations() |>
    filter(organisationNameSv == "KTH, Kungliga tekniska h\u00f6gskolan") |>
    pull(organisationId)

  kth_swecris <- swecris::swecris_projects(orgid = kth_orgid)
  #to_lower_initial <- function(x) sub('^(.)', '\\L\\1', x, perl = TRUE)

  swecris_projects_people <-
    kth_swecris |> select(projectId, peopleList) |>
    unnest_longer("peopleList") |>
    unnest_wider("peopleList")

  swecris_projects_codes <-
    kth_swecris |> select(projectId, scbs) |>
    unnest_longer("scbs") |>
    unnest_wider("scbs")

  # kth_swecris <- swecris::swecris_funding()
  # names(kth_swecris) <- to_lower_initial(names(kth_swecris))
  # names(kth_swecris)[names(kth_swecris) == "involvedPeople"] <- "peopleList"

  # people <-
  #   kth_swecris |> mutate(ip = purrr::pmap(.progress = TRUE,
  #     .l = list(InvolvedPeople),
  #     .f = swecris::parse_involved_people)) |>
  #   select(ProjectId, ip)

  # swecris_projects_people <-
  #   people |>
  #   tidyr::unnest_longer("ip", simplify = TRUE) |>
  #   tidyr::unnest("ip")
  #
  # scb_codes <-
  #   kth_swecris |>
  #   mutate(codes = pmap(.progress = TRUE,
  #     .l = list(Scbs),
  #     .f = swecris::parse_scb_codes)) |>
  #   select(ProjectId, codes)
  #
  # swecris_projects_codes <-
  #   scb_codes |>
  #   tidyr::unnest_longer("codes", indices_to = "id", simplify = TRUE) |>
  #   tidyr::unnest("codes")

  # unfortunately we cannot do this as the separator char is used not only as sep
  # swecris_projects_codes |> tidyr::separate("scb_sv_en", sep = ", ", into = c("scb_sv", "scb_en"))


  tictoc::toc()

  message("Done")

  res <- list(
    swecris_projects = kth_swecris,
    swecris_projects_people = swecris_projects_people,
    swecris_projects_codes = swecris_projects_codes
  )

  return(res)

}

#' @import OpenAIRE
kth_openaire <- function(format = c("tsv", "xml")) {

  if (!requireNamespace("OpenAIRE", quietly = TRUE)) {
    stop(
      "Package \"OpenAIRE\" must be installed to use this function.",
      call. = FALSE
    )
  }

  # XML format takes longer (200x, 3+ minutes), but provides more details
  message("Requesting projects from OpenAIRE")
  tictoc::tic()
  fmt <- match.arg(format)
  res <- OpenAIRE::openaire("projects", params = OpenAIRE::api_params(
    format = fmt,
    proj_country = "SE",
    proj_org = "Royal Institute of Technology"
  ))
  tictoc::toc()
  message("Done")
  return(res)
}

projects_upload <- function() {

  #TODO: Investigate https://github.com/rstudio/renv/commit/da7bffd747b05384a99e0d4be0bf372d0b6364a1
  #kth_cordis <- kth_cordis() #
  kth_formas <- kth_formas() # 471 project, ca 13 s
  kth_vinnova <- kth_vinnova() # 441 projects, ca 4 minutes

  sc <- kth_swecris() # < 10 s
  kth_swecris <- sc$swecris_projects # 5 secs
  kth_swecris_codes <- sc$swecris_projects_codes
  kth_swecris_people <- sc$swecris_projects_people

  kth_openaire <- kth_openaire() # 2 secs or 3 minutes, 813 projects
  kth_case <- kth_case()  # 5 secs, 3000+ projects

  #ko <- kthid_orcid()
  #ko |> readr::write_csv("/tmp/kthid_orcid.csv")
  wcsv <- function(x, f) readr::write_csv(x = x, file = f, na = "")

  kth_openaire |> wcsv("/tmp/projects_openaire.csv")
  #kth_cordis |> wcsv("/tmp/projects_cordis.csv")
  kth_vinnova |> wcsv("/tmp/projects_vinnova.csv")
  kth_formas |> wcsv("/tmp/projects_formas.csv")
  kth_swecris |> wcsv("/tmp/projects_swecris.csv")
  kth_swecris_codes |> wcsv("/tmp/projects_codes_swecris.csv")
  kth_swecris_people |> wcsv("/tmp/projects_people_swecris.csv")

  kth_case |> readr::write_csv("/tmp/projects_case.csv")

  #diva_upload_s3("/tmp/kthid_orcid.csv")
  diva_upload_s3("/tmp/projects_openaire.csv")
  #diva_upload_s3("/tmp/projects_cordis.csv")
  diva_upload_s3("/tmp/projects_vinnova.csv")
  diva_upload_s3("/tmp/projects_formas.csv")
  diva_upload_s3("/tmp/projects_swecris.csv")
  diva_upload_s3("/tmp/projects_codes_swecris.csv")
  diva_upload_s3("/tmp/projects_people_swecris.csv")
  diva_upload_s3("/tmp/projects_case.csv")

}
#' @importFrom readr read_csv read_csv2 read_delim
#' @importFrom tictoc tic toc
#'
kth_case <- function() {

  message("Wranging projects data from CASE")
  tictoc::tic()

  Diva_org_id <- `Project ID` <- beg <- end <- slug <- unit_code <- unit_long_en <-
    unit_short  <- NULL

  # parse and remap colnames; use lowersnakecase field names
  # to fix R pkg warn: esc <- function(x) cat(stringi::stri_escape_unicode(x))

  case_mapping <- readr::read_csv(show_col_types = FALSE, "colname,export
    Project ID,display_project_id
    Name,name
    Funding Organisation,funding_org
    school,responsible_school
    Primary Researcher,primary_researcher
    Project Number,project_number
    Agresso Number,agresso_number
    Start Date,project_start_date
    End Date,project_end_date
    sdg,un_dev_goals
    Role,role
    dep,department
    Other participating Schools,other_schools
    username,primary_researcher:active_directory_account
    efecte_id,efecte_id
    Status,status
    program,program
    subprogram,subprogram
    subprogram_category,subprogram_category
    subprogram_category_description,subprogram_category_description
    co_funding_org,co_funding_org
    kth_grant_amount,kth_grant_amount
    related_project,related_project
    external_coordinator,external_coordinator
    counterpart,counterpart
    type,type
    cost_center_school,responsible_school:cost_center_id
    cost_center_dep,department:cost_center_id
    cost_center_other,other_schools:cost_center_id
    ")

  dep <-
    mc_read("kthb/case/case_dept_match.csv") |>
    read_delim(";", locale = readr::locale(encoding = "UTF-8"), show_col_types = FALSE) |>
    select(dep, dep_slug = slug, dep_code = unit_code, dep_divaorg = Diva_org_id,
      dep_short = unit_short, dep_desc = unit_long_en)

  school <-
    mc_read("kthb/case/case_school_match.csv") |>
    read_delim(";", locale = readr::locale(encoding = "UTF-8"), show_col_types = FALSE) |>
    select(school, school_slug = slug, school_code = unit_code, school_divaorg = Diva_org_id,
           school_short = unit_short, school_desc = unit_long_en)

  f <- paste0(gsub("-", "", substr(lubridate::today(), start = 3, stop = 10)), "_case", ".csv")
  file <- mc_read(paste0("kthb/case/", f))

  case <-
    readr::read_csv2(file, show_col_types = FALSE, guess_max = 3e3,
      locale = readr::locale(grouping_mark = ".", decimal_mark = ","))

  mapping <- case_mapping

  case_map <- function(x) {
    tibble(export = x) |>
      inner_join(mapping, by = "export") |>
      pull("colname")
  }

  # fix issues with added fields not present in mapping, ie
  # "Names must be unique. Names are duplicated.
  #  In names[cols] <- .fn(names[cols], ...) :
  #  number of items to replace is not a multiple of replacement length"

  cn <- colnames(case)

  if (length(cn) > nrow(mapping)) {
    mismatch <- setdiff(colnames(case), mapping$export)
    warning("New fields have been added, not present in case_mapping: ",
            paste0(collapse = ", ", mismatch))
    case <- case |> select(-any_of(mismatch))
  }

  case <-
    case |>
    rename_with(.fn = case_map, .cols = any_of(cn))

  # data types parsing

  #intcols <- c("kth_grant_amount")
  dtecols <- c("Start Date", "End Date")

  typed <-
    case |>
    mutate(across(.cols = contains(dtecols), .fns = function(x)
      readr::parse_date(substr(x, 1, 10), format = "%Y-%m-%d")))

  probs <- purrr::map_dfr(typed |> select(any_of(c(dtecols))), readr::problems)

  if (nrow(probs) > 0) {
    print(probs)
    info <- case |> select(`Project ID`, c(dtecols)) |>
      slice(probs$row) |> mutate(row = probs$row) |>
      inner_join(probs, by = "row") |>
      select(`Project ID`, row, everything())
    w <- paste0(collapse = "\n", capture.output(info))
    warning("Proceeding, but with parsing issues for row(s): ",
      paste(sep = ", ", probs$row), "\n\n", w, "\n")
    w2 <- readr::read_lines(file)[probs$row + 1]
    warning("Raw data:\n\n", w2)
  }

  res <- typed |>
    rename(
      beg = "Start Date",
      end = "End Date"
    ) |>
    mutate(
      duration = end - beg
    ) |>
    left_join(dep, by = "dep") |>
    left_join(school, by = "school")

  tictoc::toc()
  message("Done wrangling CASE data")

  return(res)
}

#' @importFrom arrow write_parquet
#' @importFrom readr read_csv
#' @importFrom purrr map_lgl
refresh_projects_bucket <- function() {

  # use previously installed mc with minioclient 0.0.5
  options("minioclient.dir" = dirname(Sys.which("mc")))

  fn <- NULL

  project_files <-
    mc_ls("kthb/kthcorpus") |>
    grep(pattern = "projects?_.*?\\.csv$", value = TRUE)

  td <- file.path(tempdir(check = TRUE), "projects")

  if (!dir.exists(td)) dir.create(td, recursive = TRUE)

  sync_file <- function(x, bucket = "kthb/kthcorpus/", tempdir = tempdir()) {
    if (!dir.exists(tempdir)) dir.create(tempdir, recursive = TRUE)
    from <- paste0(bucket, x)
    destfile <- x |> gsub(pattern = "\\.csv", replacement = "\\.parquet")
    to <- file.path(tempdir, destfile)
    csv <- mc_read(from) |> readr::read_csv(show_col_types = FALSE)
    message("Writing ", from, " to ", to)
    arrow::write_parquet(csv, to)
    file.exists(to)
  }

  is_converted <-
    project_files |>
    map_lgl(function(x) sync_file(x, tempdir = td), .progress = TRUE)

  stopifnot(all(is_converted))

  minioclient::mc_mirror(td, "kthb/projects", overwrite = TRUE, verbose = TRUE)

}


# # frequency of associated people
# # 21% of projects have no info about Principal Investigator
# kth_swecris |>
#   left_join(
#     kth_swecris |>
#     unnest(peopleList) |>
#     select(projectId, peopleList) |>
#     unnest_wider(peopleList) |>
#       filter(roleEn == "Principal Investigator") |>
#     group_by(projectId) |>
#     count() |>
#     arrange(desc(n))
#   ) |>
#   group_by(projectId) |>
#   summarize(n = sum(n)) |>
#   pull(n) |> table(useNA = "ifany")

