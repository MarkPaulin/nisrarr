new_nisra_df <- function(x = list(), n = NULL, meta = NULL, ..., class = NULL) {
  out <- vctrs::new_data_frame(x, n = n, ..., class = c("nisra_df", class))
  attr(out, "meta") <- meta
  out
}

nisra_df <- function(..., meta = NULL) {
  data <- vctrs::df_list(...)
  new_nisra_df(data, meta = meta, class = c("tbl_df", "tbl"))
}

#' Get metadata
#'
#' Retrieve metadata from a dataset, such as contact information, notes, and
#' official statistics status.
#'
#' TODO: details on required fields
#'
#' @param x A dataset created using [nisra_read_dataset()]
#' @param field The metadata field to read. See details for a list of fields.
#'
#' @return Metadata if the field is found, otherwise `NULL`
#' @export
get_metadata_field <- function(x, field) {
  UseMethod("get_metadata_field")
}

#' @exportS3Method
get_metadata_field.default <- function(x, field) {
  x_type <- obj_type_friendly(x)
  stop(paste("`get_metadata_field` not implemented for", x_type))
}

#' @exportS3Method
get_metadata_field.nisra_df <- function(x, field) {
  attr(x, "meta", exact = TRUE)[[field]]
}

#' @exportS3Method pillar::tbl_format_footer
tbl_format_footer.nisra_df <- function(x, setup, ...) {
  default_footer <- NextMethod()

  if (!"label" %in% names(attr(x, "meta", exact = TRUE))) {
    return(default_footer)
  }

  source_info <- paste0("Source: ", get_metadata_field(x, "label"))
  source_footer <- pillar::style_subtle(paste0("# ", source_info))
  c(default_footer, source_footer)
}
