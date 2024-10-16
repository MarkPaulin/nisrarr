test_that("nisra_data_portal_request uses parameters", {
  vcr::use_cassette("nisra_data_portal_request_uses_parameters", {
    resp <- nisra_data_portal_request("test_method", params = list(made_up_parameter = "test"))
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
