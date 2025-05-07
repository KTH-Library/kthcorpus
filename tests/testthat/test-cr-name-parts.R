test_that("name parts can be retrieved from crossref dois", {
  skip_on_ci()
  nps <- rep("10.1021/cr900356p", 2) |> cr_name_parts()
  is_valid <- length(nps$author_raw_name) == 5
  expect_true(is_valid)

})
