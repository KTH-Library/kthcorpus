test_that("scopus search works", {
  skip_on_ci()
  ss <- scopus_search_pubs_kth()
  s <- ss$publications
  is_valid <- nrow(s) >= 1
  expect_true(is_valid)
})

test_that("scopus remaining quota works", {
  skip_on_ci()
  rl <- scopus_ratelimit_quota()
  is_valid <- !any(is.null(c(rl$`X-RateLimit-Limit`, rl$`X-RateLimit-Remaining`, rl$`X-RateLimit-Reset`)))
  expect_true(is_valid)
})

test_that("scopus abstract extended info works", {
  skip_on_ci()
  mysid <- "SCOPUS_ID:85140456799"
# SCOPUS_ID:85140456799 - no org_sourcetext?
# SCOPUS_ID:85140456964 - no correspondence?
  ae <- scopus_abstract_extended(mysid)
  is_invalid <- ae$scopus_correspondence$ce_surname |> is.na() |> all()
  expect_true(!is_invalid)
})

test_that("scopus abstract extended removes ror strings from org aff", {

  skip_on_ci()

  mysid <- "SCOPUS_ID:85204443832"

  ae <- scopus_abstract_extended(mysid)

  is_invalid <- 
    any(ae$scopus_authorgroup$raw_org |> 
      grepl(pattern = "\\s*https://ror\\.org/[0-9a-z]{9}"))
  
  expect_true(!is_invalid)
})


test_that("scopus abstract extended info works for specific eid", {
  skip_on_ci()
  my_eid <- "2-s2.0-85187216462"

  mods_from_eid <- function(eid) {
    scopus_mods_params(
      scopus = scopus_search_id(eid),
      sid = gsub("2-s2.0-", "", eid)
    ) |>
    create_diva_mods()
  }

  my_string <- mods_from_eid(my_eid)

  is_invalid <- my_string |> grepl(pattern = "country=")

  expect_true(!is_invalid)
})


test_that("scopus abstract extended extraction of affiliations does not contain duplicates for specific eid", {
  
  skip_on_ci()
  
  my_eid <- "2-s2.0-85180686613"

  mods_from_eid <- function(eid) {
    scopus_mods_params(
      scopus = scopus_search_id(eid),
      sid = gsub("2-s2.0-", "", eid)
    ) |>
    create_diva_mods()
  }

  my_string <- mods_from_eid(my_eid)

  my_xml <- xml2::as_xml_document(my_string)
  
  xml2::xml_ns_strip(my_xml) 
  
  is_invalid <- 
    any(
      my_xml |> 
      xml2::xml_find_all("//affiliation") |>
      as.character() |> 
      stringr::str_count("Viale") > 1
    )
  
  expect_true(!is_invalid)
})
