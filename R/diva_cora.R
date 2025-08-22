cora_base <- "https://cora.diva-portal.org/diva/rest/record/"

cora_organisation_search <- function(domain = "kth", beg = 1, end = 800,
                                     freetext, orgid, orgname, verbose = FALSE) {

  myglue <- function(x) glue::glue(.open = "#{", .close = "}#", x)

  if (missing(freetext)) freetext <- NULL
  if (missing(orgid)) orgid <- NULL
  if (missing(orgname)) orgname <- NULL

  t_domain <- curl::curl_escape(domain)
  t_freetext <- curl::curl_escape(freetext)
  t_orgid <- curl::curl_escape(orgid)
  t_orgname <- curl::curl_escape(orgname)

  p_domain <- myglue('{"name":"divaOrganisationDomainSearchTerm","value":"#{t_domain}#"}')
  p_freetext <- myglue('{"name":"organisationGeneralSearchTerm","value":"#{t_freetext}#"}')
  p_orgid <- myglue('{"name":"recordIdSearchTerm","value":"#{t_orgid}#"}')
  p_orgname <- myglue('{"name":"organisationNameSearchTerm","value":"#{t_orgname}#"}')

  terms <- paste0(collapse = ",", c(p_freetext, p_domain, p_orgname, p_orgid))

  param <- glue::glue(.open = "#{", .close = "}#",
    paste0(
      '{"name":"search","children":[{"name":"include",',
      '"children":[{"name":"includePart","children":', '[',
        terms,
      ']}]},',
      '{"name":"start","value":"#{beg}#"},{"name":"rows","value":"#{end}#"}]}')
    )

  endpoint <- paste0(cora_base,
    "searchResult/publicOrganisationSearch?searchData=", param
  )

  if (verbose) {
    message("Sending request to: \n")
    cat(endpoint)
    message("\n")
  }

  res <- httr::GET(endpoint)

  if (httr::status_code(res) != 200) {
    #print(res)
    stop("Error: ", httr::content(res, type = "text/plain", encoding = "UTF-8"))
  }

  httr::content(res, type = "application/json", encoding = "UTF-8")
}

cora_organisation <- function(identifier) {

  param <- curl::curl_escape(identifier)

  endpoint <- paste0(cora_base,
    "subOrganisation/", param
  )

  message("Requesting: ")
  cat(endpoint)

  #TODO: catch errors like
  #Error reading one row using sql: select * from suborganisationview where id = ?

  res <- httr::GET(endpoint)

  if (httr::status_code(res) != 200) {
    #print(res)
    stop("Error from CORA API: ", httr::content(res, type = "text/plain", encoding = "UTF-8"))
  }

  httr::content(res, type = "application/json", encoding = "UTF-8")
}

# cora_person_search <- function(searchterm = "", beg = 1, end = 800) {
#
#   term <- curl::curl_escape(searchterm)
#
#   param <- glue::glue(.open = "#{", .close = "}#",
#     paste0(
#       '{"name":"search","children":[{"name":"include",',
#       '"children":[{"name":"includePart","children":',
#       '[{"name":"personNameSearchTerm","value":"#{term}#"}]}]}]}')
#     )
#
#   endpoint <- paste0(cora_base,
#     "searchResult/publicPersonSearch?searchData=", param
#   )
#
#  httr::content(httr::GET(endpoint), type = "application/json")
#
# }

cora_person_search <- function(freetext, id, name, beg = 1, end = 800, verbose = FALSE) {

   myglue <- function(x) glue::glue(.open = "#{", .close = "}#", x)

  if (missing(freetext)) freetext <- NULL
  if (missing(id)) id <- NULL
  if (missing(name)) name <- NULL

  t_freetext <- curl::curl_escape(freetext)
  t_id <- curl::curl_escape(id)
  t_name <- curl::curl_escape(name)


  p_freetext <- myglue('{"name":"personGeneralSearchTerm","value":"#{t_freetext}#"}')
  p_name <- myglue('{"name":"personNameSearchTerm","value":"#{t_name}#"}')
  p_id <- myglue('{"name":"personIdSearchTerm","value":"#{t_id}#"}')

  terms <- paste0(collapse = ",", c(p_freetext, p_name, p_id))

  param <- glue::glue(.open = "#{", .close = "}#",
    paste0(
      '{"name":"search","children":[{"name":"include",',
      '"children":[{"name":"includePart","children":', '[',
        terms,
      ']}]},',
      '{"name":"start","value":"#{beg}#"},{"name":"rows","value":"#{end}#"}]}')
    )

  endpoint <- paste0(cora_base,
    "searchResult/publicPersonSearch?searchData=", param
  )

  if (verbose) {
    message("Sending request to: \n")
    cat(endpoint)
    message("\n")
  }

  res <- httr::GET(endpoint)

  if (httr::status_code(res) != 200) {
    #print(res)
    stop("Error: ", httr::content(res, type = "text/plain", encoding = "UTF-8"))
  }

  httr::content(res, type = "application/json", encoding = "UTF-8")
}

cora_person <- function(identifier) {

  param <- curl::curl_escape(identifier)

  endpoint <- paste0(cora_base,
    "person/authority-person:", param
  )

 httr::content(httr::GET(endpoint), type = "application/json")
}



#cora_organisation_search()
#cora_organisation("5850")
#cora_person_search("Anders Wändahl")
#cora_person(54882)

#' @importFrom rrapply rrapply
flatten_cora_records <- function(coras) {

  boho <- path <- value <- is_name <- is_value <- nme <- val <- p <- NULL

  t0 <-
    rrapply::rrapply(coras,
      condition = function(x) !is.na(x),
      f = \(x) x,
      how = "melt",
      options = list(namecols = TRUE)
    ) %>%
    tidyr::unite("boho", -c("value"), na.rm = TRUE) %>%
    tibble::as_tibble() %>%
    mutate(is_name = grepl("_name$", boho), is_value = grepl("_value$", boho)) %>%
    mutate(path = gsub("(_name$)|(_value$)", "", boho)) %>%
    select(path, value, is_name, is_value) %>%
    mutate(val = ifelse(is_value, value, NA), nme = ifelse(is_name, value, NA))

  t1 <- t0 %>% filter(!is.na(nme)) %>% distinct(path, nme)
  t2 <- t0 %>% filter(!is.na(val)) %>% distinct(path, val)

  t3 <-
    t1 %>% inner_join(t2, by = "path") %>%
    rowwise() %>%
    mutate(p = paste0(collapse = "_", unlist(stringr::str_match_all(path, "\\d+")))) %>%
    ungroup() %>%
    select(p, nme, val) %>%
    arrange(p) %>%
    mutate(id = ifelse(grepl("id", nme), val, NA)) %>%
    tidyr::fill(id, .direction = "down") %>%
    distinct(id, nme, val) %>%
    filter(nme != "id", id != "177") %>%
    tidyr::pivot_wider(
      values_from = val,
      names_from = nme,
      names_repair = "unique",
      values_fn = function(x) paste0(collapse = "|", x)
    )

  t3
}

cora_fixup_organisations <- function(data) {

  linkedRecordType <- linkedRecordId <-
    closedDate <- closed_date <- internalNote <- l_orgid <- name_1 <- name_2 <-
    org_code <- org_type <- organisationCode <- organisationType <-
    orgid <- p_orgid <- tsCreated <- tsUpdated <- ts_created <- ts_updated <-
    unit_en <- unit_sv <- org_code <- NULL

  data |>
    tidyr::separate("name", c("name_1", "name_2"), sep = "[|]", fill = "right") |>
    tidyr::separate("language", c("lang_1", "lang_2"), sep = "[|]", fill = "right") |>
    mutate(linkedRecordType = gsub("recordType|system|coraUser|", "", linkedRecordType, fixed = TRUE)) |>
    rowwise() |>
    mutate(linkedRecordId = paste0(collapse = ",", unlist(stringr::str_extract_all(linkedRecordId, "\\d+")))) |>
    ungroup() |>
    mutate(linkedRecordId = gsub("4412982402853626,", "", linkedRecordId)) |>
    mutate(across(starts_with("ts"), parse_ts)) |>
    rename(
      orgid = id,
      level_desc = linkedRecordType,
      l_orgid = linkedRecordId,
      ts_created = tsCreated,
      ts_updated = tsUpdated,
      unit_sv = name_1,
      unit_en = name_2,
      closed_date = closedDate,
      org_type = organisationType,
      note = internalNote,
      org_code = organisationCode
    ) |>
    select(-c("selectable", "domain")) |>
    mutate(orgid = as.integer(orgid)) |>
    mutate(p_orgid = as.integer(sapply(strsplit(l_orgid, ","), "[[", 1))) |>
    select(
      orgid, org_type, p_orgid, org_code, l_orgid,
      unit_sv, unit_en, org_code,
      closed_date, ts_created, ts_updated
    )
}

parse_ts <- function(x)
  gsub("(.*?)Z", "\\1", x) |>
  strptime(format = "%Y-%m-%dT%H:%M:%OS")

#' Search DiVA organisations using CORA API
#' @param domain string with domain, by default "kth"
#' @param freetext string with freetext to search for
#' @returns data frame with results
#' @export
diva_organisations_cora <- function(domain = "kth", freetext) {

  cos <-
    cora_organisation_search(
      verbose = FALSE,
      domain = domain,
      freetext = freetext
    )

  cos |>
    flatten_cora_records() |>
    cora_fixup_organisations()
}

cora_fixup_persons <- function(data) {

#  print(data)

  domain <- familyName <- givenName <- public <- tsCreated <- NULL

  data |>
    mutate(ts = parse_ts(tsCreated)) |>
    tidyr::separate_longer_delim(cols = c("givenName"), delim = "|") |>
    tidyr::separate_longer_delim(cols = c("familyName"), delim = "|") |>
    mutate(name_variation = paste0(familyName, ", ", givenName)) |>
    select(any_of(c("id", "ts", "public", "domain", "name_variation", "ORCID_ID", "academicTitle", "URL"))) |>
    rename_with(.fn = \(x) ifelse(x == "ORCID_ID", "orcid", x)) |>
    rename_with(.fn = \(x) ifelse(x == "academicTitle", "title", x)) |>
    rename_with(.fn = \(x) ifelse(x == "URL", "url", x)) |>
    rename_with(.fn = \(x) ifelse(x == "id", "author_id", x)) |>
    mutate(orcid = ifelse("orcid" %in% colnames(data), replace(orcid, orcid == "", NA_character_), NA_character_)) |>
    mutate(public = ifelse(public == "yes", TRUE, FALSE))

}

diva_persons_cora <- function(search_term, my_domain = NULL) {

  domain <- NULL

  persons <-
    cora_person_search(name = search_term, verbose = FALSE) |>
    flatten_cora_records()

  if (nrow(persons) == 0) return(data.frame())

  persons <- persons |> cora_fixup_persons()

  if (!is.null(my_domain))
    persons <- persons |> dplyr::filter(my_domain == domain)

  persons
}

