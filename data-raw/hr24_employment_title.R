hr24_employment_title <- readr::read_csv("data-raw/hr24_employment_title.csv")
usethis::use_data(hr24_employment_title, overwrite = TRUE)

# For updates:
# - edit csv file
# - run this
# - commit and push
