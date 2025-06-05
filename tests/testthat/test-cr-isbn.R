test_that("looking up an ISBN using crossref works", {
  skip_on_ci()
  res <- "9783030755379" |> cr_isbn_lookup()
  is_valid <- nrow(res) > 50 & ncol(res) == 3
  expect_true(is_valid)
})
