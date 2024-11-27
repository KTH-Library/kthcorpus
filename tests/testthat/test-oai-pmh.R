library(dbplyr)

test_that("connection through OAI-PMH can be made", {
  skip_on_ci()
  hello <- oai_hello()
  is_valid <- nrow(hello$sets) > 20 
  expect_true(is_valid)
})

test_that("database exists w harvest", {
  skip_on_ci()
  con <- oai_con()
  on.exit(DBI::dbDisconnect(con))
  is_valid <- "ids" %in% DBI::dbListTables(con)
  expect_true(is_valid)
})

test_that("changes since latest timestamp from database can be retrieved", {

  skip_on_ci()

  changes <- oai_changes()
  n_rows <- changes |> map_int(nrow)
  
  is_ok <- length(unique(n_rows)) == 1
  expect_true(is_ok)

})

test_that("reading from oai db works using duckdb", {

  skip()

  con <- oai_con()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  # TODO: this causes a core dump!
  con |> DBI::dbGetQuery(
    "from (from mods_extra where length(json) > 10 limit 10) 
    select json_extract_path(json::JSON, 
    '$.mods.recordInfo.recordIdentifier.\"#\"')::VARCHAR as PID;"
  )

})

test_that("reading from s3 authed works using duckdb", {

  skip_on_ci()

  is_valid <- 
    duckdb_s3_query("from 's3://kthcorpus/ug_kthid_orcid.csv' limit 5") |> 
    nrow() == 5

  expect_true(is_valid)
  
})

test_that("getting ten latest records as tables works", {

  skip()

  oai_db_ten_latest <- function(con) {
    my_jsons <- 
      con  |> tbl("ids")  |> arrange(desc(datestamp))  |> head(100) |> 
      left_join(by = join_by("identifier"), con  |> tbl("mods_extra"))  |> arrange(desc(datestamp))  |> 
      filter(is.na(status)) |> 
      pull(json)
    
  #  json_40 <- my_jsons[40]
  
    my_list <- 
      my_jsons |> 
      map(\(x) mods_json_to_object(x) |> 
        possibly(parse_mods_object)())  |> 
      compact() 
    
    # "titleInfo": [
    #   {
    #     "#": "",
    #     "@lang": "eng",
    #     "title": {
    #       "#": "Development of a computational model for creep crack growth"
    #     }
    #   },
    #   {
    #     "#": "",
    #     "@lang": "swe",
    #     "title": {
    #       "#": "Framtagning av beräkningsmodell för krypspricktillväxt"
    #     },
    #     "@type": "alternative"
    #   }
    # ]
  
    my_jsons[40] |> fromJSON() |> toJSON(pretty = TRUE)
    my_list[1:5] |> 
      mods_records_to_tbls()
  }

  expect_true(TRUE)
  
})