#' Attach boundaries to a dataset
#'
#' @param x A `nisra_df` dataset
#' @return An `sf` object with all the columns of `x` and an additional geometry
#' column.
nisra_get_boundaries <- function(x) {
  rlang::check_installed("sf", reason = "to use `nisra_get_boundaries()`")

  geo_var <- get_metadata_field(x, "role")[["geo"]][[1]]
  if (is.null(geo_var)) {
    cli::cli_abort("No geography found!")
  }

  geo_dim <- get_metadata_field(x, "dimension")[[geo_var]]
  geo_link <- geo_dim[["link"]][["enclosure"]][[1]][["href"]]
  boundaries <- sf::read_sf(geo_link)

  lang <- get_metadata_field(x, "language")[["code"]]
  label <- geo_dim[["label"]]

  colnames(boundaries) <- dplyr::case_match(
    colnames(boundaries),
    lang ~ label,
    "code" ~ geo_var,
    .default = colnames(boundaries)
  )

  if (geo_var %in% colnames(x)) {
    out <- dplyr::right_join(boundaries, x, by = geo_var)
  } else {
    out <- dplyr::right_join(boundaries, x, by = label)
  }

  out <- out[, c(colnames(x), "geometry")]

  out
}
