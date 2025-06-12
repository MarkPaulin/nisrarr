vcr::use_cassette("geography", {
  test_that("nisra_get_boundaries works", {
    skip_if_not_installed("sf")

    x <- nisra_read_dataset("BSAA")
    y <- nisra_get_boundaries(x)

    expect_true("geometry" %in% colnames(y))
    expect_s3_class(x, c("sf", "nisra_df"))

    # remove when this is exposed to the user
    labels <- get_metadata_field(x, "dimension")[["AA"]][["category"]][[
      "label"
    ]]
    aa_lkp <- setNames(names(labels), purrr::list_c(labels))
    x[["AA"]] <- aa_lkp[x[["Assembly Area"]]]
    x[["Assembly Area"]] <- NULL
    y2 <- nisra_get_boundaries(x)
    expect_true("geometry" %in% colnames(y))
    expect_s3_class(x, c("sf", "nisra_df"))

    mt <- get_metadata(x)
    mt[["role"]][["geo"]] <- NULL
    attr(x, "meta") <- mt
    expect_error(nisra_get_boundaries(x))
  })
})
