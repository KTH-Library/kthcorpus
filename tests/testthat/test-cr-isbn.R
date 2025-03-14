test_that("looking up an ISBN using crossref works", {
  res <- "9783030755379" |> cr_isbn_lookup()
  is_valid <- nrow(res) > 80 & ncol(res) == 3
  expect_true(is_valid)
})
