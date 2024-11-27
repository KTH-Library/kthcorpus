test_that("hr_plus works with data from three days back", {
  skip_on_ci()
  d1 <- fetch_hr24() |> dplyr::as_tibble()
  message("The following fields are missing when comparing to hrplus:\n", 
    paste(collapse = "\n", setdiff(hr_mapping$colname, names(d1))))
  
  d1$emp_lastmod <- NA_character_
  d1$is_public <- NA_character_
  d1$emp_title_eng <- NA_character_
  d1$unit_status <- NA_character_

  #setdiff(colnames(d1), hr_mapping$colname)

  is_valid <- nrow(d1) > 1000 &&   length(setdiff(hr_mapping$colname, colnames(d1))) == 0

  expect_true(is_valid)
})
