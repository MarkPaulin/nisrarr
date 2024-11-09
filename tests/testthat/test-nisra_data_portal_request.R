test_that("nisra_data_portal_request uses parameters", {
  vcr::use_cassette("nisra_data_portal_request_uses_parameters", {
    resp <- nisra_data_portal_request(
      "test_method",
      params = list(made_up_parameter = "test")
    )
  })

  query_data <- httr2::resp_url_queries(resp)[["data"]] |>
    jsonlite::fromJSON()

  expect_equal(
    query_data,
    list(
      jsonrpc = "2.0",
      method = "test_method",
      params = list(made_up_parameter = "test")
    )
  )
})


vcr::use_cassette("nisra_data_portal_application_error", {
  test_that("nisra_data_portal reports application errors", {
    expect_error(
      resp <- nisra_data_portal("incorrect_method", flush_cache = TRUE),
      "Error from server"
    )
  })
})


vcr::use_cassette("nisra_search", {
  test_that("nisra_search finds the right keywords", {
    out <- nisra_search(
      "health",
      datefrom = "2024-10-14",
      flush_cache = TRUE,
      regex = "\\w"
    )

    expect_equal(
      names(out),
      c(
        "dataset_code", "dataset_label", "frequency",
        "dataset_dimensions", "updated"
      )
    )
    labels <- out[["dataset_label"]]
    expect_true(all(grepl("health", labels, ignore.case = TRUE)))
  })
})

vcr::use_cassette("nisra_search_2", {
  test_that("nisra_search checks for dimensions", {
    out <- nisra_search(
      datefrom = "2024-10-14",
      flush_cache = TRUE,
      variables = "age"
    )

    data_dims <- out$dataset_dimensions
    expect_true(
      all(vapply(data_dims, \(x) {
        any(stringr::str_detect(x, stringr::fixed("age", ignore_case = TRUE)))
      }, logical(1)))
    )
  })
})

vcr::use_cassette("nisra_read_dataset", {
  test_that("datasets are returned as tibbles", {
    df <- nisra_read_dataset("CCMLGD", flush_cache = TRUE)

    expect_s3_class(df, "tbl_df")
    expect_identical(
      colnames(df),
      c("Statistic Label", "Month", "Local Government District", "UNIT", "VALUE")
    )
  })
})
