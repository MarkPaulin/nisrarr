nisra_data_portal <- function(method, ...) {
  params <- list(...)
  query_data <- list(
    jsonrpc = "2.0",
    method = method,
    params = params
  )

  query_json <- jsonlite::toJSON(query_data, auto_unbox = TRUE)
  httr2::request("https://ws-data.nisra.gov.uk/public/api.jsonrpc") |>
    httr2::req_url_query(data = query_json) |>
    httr2::req_user_agent("nisrarr (http://github.com/MarkPaulin/nisrarr)") |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = TRUE)
}

#' Read NISRA data portal dataset
#'
#' @param dataset_code Dataset code
#'
#' @return
#' @export
nisra_read_dataset <- function(dataset_code) {
  response <- nisra_data_portal(
    "PXStat.Data.Cube_API.ReadDataset",
    class = "query",
    id = list(),
    dimension = c(),
    extension = list(
      pivot = NA,
      codes = FALSE,
      language = list(
        code = "en"
      ),
      format = list(
        type = "CSV",
        version = "1.0"
      ),
      matrix = dataset_code
    ),
    version = "2.0"
  )

  readr::read_csv(response[["result"]])
}
