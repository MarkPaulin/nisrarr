nisra_data_portal_request <- function(method, params) {
  query_data <- list(
    jsonrpc = "2.0",
    method = method,
    params = params
  )

  query_json <- jsonlite::toJSON(query_data, auto_unbox = TRUE)

  req <- httr2::request("https://ws-data.nisra.gov.uk/public/api.jsonrpc") |>
    httr2::req_throttle(10 / 60) |>
    httr2::req_url_query(data = query_json) |>
    httr2::req_user_agent("nisrarr (http://github.com/MarkPaulin/nisrarr)")

  resp <- req |>
    httr2::req_perform()

  resp
}


nisra_data_portal <- function(method, ..., flush_cache = FALSE) {
  cache <- cachem::cache_disk(path.expand("~/.nisrarr"), max_age = 60 * 60 * 24)
  params <- list(...)
  key <- rlang::hash(list(method, params))

  if (cache$exists(key) && !flush_cache) {
    resp <- cache$get(key)
    resp_body <- resp |>
      httr2::resp_body_json(simplifyVector = TRUE, simplifyDataFrame = FALSE)

    return(resp_body)
  }

  resp <- nisra_data_portal_request(method, params)

  resp_body <- resp |>
    httr2::resp_body_json(simplifyVector = TRUE, simplifyDataFrame = FALSE)

  if ("error" %in% names(resp_body)) {
    msg <- resp_body[["error"]][["message"]]
    cli::cli_abort(c(
      "Error from server!",
      "i" = msg
    ))
  }

  cache$set(key, resp)

  resp_body
}


#' Read NISRA data portal dataset
#'
#' Fetch a dataset from the NISRA data portal using the dataset code. You
#' can search for a dataset using [nisra_search()].
#'
#' @param dataset_code Dataset code
#' @param flush_cache Ignore cached values
#'
#' @return A [tibble::tibble()] with the dataset.
#' @export
#'
#' @example
#' \dontrun{
#'
#' }
nisra_read_dataset <- function(dataset_code, flush_cache = FALSE) {
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
    version = "2.0",
    flush_cache = flush_cache
  )

  if (is.null(response[["result"]])) {
    cli::cli_abort(c(
      "Unable to find dataset: {dataset_code}",
      "i" = "You can check the code for the dataset using {.fn nisra_search}"
    ))
  }

  readr::read_csv(response[["result"]], show_col_types = FALSE)
}


nisra_read_collection <- function(datefrom = NULL, flush_cache = FALSE) {
  if (missing(datefrom) || is.null(datefrom)) {
    datefrom <- lubridate::today() - lubridate::dmonths(3)
    datefrom <- format(datefrom, format = "%Y-%m-%d")
  }

  resp <- nisra_data_portal(
    "PxStat.Data.Cube_API.ReadCollection",
    language = "en",
    datefrom = datefrom,
    flush_cache = flush_cache
  )

  codes <- vapply(resp$result$link$item, \(x) {
    x[["extension"]][["matrix"]]
  }, character(1))

  labels <- vapply(resp$result$link$item, \(x) {
    x[["label"]]
  }, character(1))

  frequencies <- vapply(resp$result$link$item, \(x) {
    dims <- x[["dimension"]]
    dplyr::coalesce(
      dims[["TLIST(A1)"]][["label"]],
      dims[["TLIST(H1)"]][["label"]],
      dims[["TLIST(Q1)"]][["label"]],
      dims[["TLIST(M1)"]][["label"]],
      dims[["TLIST(W1)"]][["label"]],
      dims[["TLIST(D1)"]][["label"]]
    )
  }, character(1))

  dimensions <- lapply(resp$result$link$item, \(x) {
    dims <- x[["dimension"]]
    vapply(dims, \(dim) {
      dim[["label"]]
    }, character(1))
  })

  updated_dates <- lubridate::ymd_hms(vapply(resp$result$link$item, \(x) {
    x[["updated"]]
  }, character(1)))

  tibble::tibble(
    dataset_code = codes,
    dataset_label = labels,
    frequency = frequencies,
    dataset_dimensions = dimensions,
    updated = updated_dates
  )
}



#' Search for a NISRA dataset
#'
#' Search the NISRA data portal for a dataset. You can search dataset titles
#' either for a keyword or with a regular expression, using a dataset code, or
#' by variables that appear in dataset. You can also specify how recently the
#' dataset must have been updated.
#'
#' @param keyword Text to search for in dataset titles
#' @param regex Regular expression for searching dataset titles
#' @param dataset_code Dataset to find
#' @param variables Variables to search for in datasets
#' @param datefrom Date to search from. Search is limited to datasets updated in
#' the last three months if not specified.
#' @param flush_cache Ignore cached values
#'
#' @return A [tibble::tibble()] of dataset information matching the search terms
#' @export
nisra_search <- function(keyword = NULL,
                         regex = NULL,
                         dataset_code = NULL,
                         variables = NULL,
                         datefrom = NULL,
                         flush_cache = FALSE) {
  coll <- nisra_read_collection(datefrom = datefrom, flush_cache = flush_cache)
  if (!missing(keyword)) {
    pattern <- stringr::fixed(keyword, ignore_case = TRUE)
    coll <- coll[stringr::str_detect(coll[["dataset_label"]], pattern), ]
  }

  if (!missing(regex)) {
    pattern <- stringr::regex(regex, ignore_case = TRUE)
    coll <- coll[stringr::str_detect(coll[["dataset_label"]], regex), ]
  }

  if (!missing(dataset_code)) {
    coll <- coll[coll[["dataset_code"]] == dataset_code, ]
  }

  if (!missing(variables)) {
    found <- vapply(coll[["dataset_dimensions"]], \(x) {
      all(vapply(variables, \(y) {
        any(stringr::str_detect(
          stringr::str_to_lower(x),
          stringr::coll(y, ignore_case = TRUE)
        ))
      }, logical(1)))
    }, logical(1))

    coll <- coll[found, ]
  }

  coll
}

nisrarr_clear_cache <- function() {
  cache <- cachem::cache_disk(path.expand("~/.nisrarr"))
  cache$reset()
}
