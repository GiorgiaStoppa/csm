context("test-continuous_anomal_values")

test_that("Class of output is correct", {

    x <- rnorm(n = 10L, mean = 10, sd = 2)
    range   <- c(3, 12)

    expect_is(
        continuous_anomal_values(x = x, range = range),
        class = "numeric"
    )

})

test_that("Class of study_data is ok", {

    url     <- "https://bbmc.ouhsc.edu/redcap/api/"
    token   <- "9A81268476645C4E5F03428B8AC3AA7B"

    expect_is(
        read_redcap(token = token, url = url)[["study_data"]],
        class = "data.frame"
    )

})

test_that("Class of the study_metadata is correct", {

    url     <- "https://bbmc.ouhsc.edu/redcap/api/"
    token   <- "9A81268476645C4E5F03428B8AC3AA7B"

    expect_is(
        read_redcap(token = token, url = url)[["study_metadata"]],
        class = "data.frame"
    )

})

test_that("Error if the token is not a string", {

    url     <- "https://bbmc.ouhsc.edu/redcap/api/"
    token   <- 876

    expect_error(read_redcap(token = token, url = url))

})

test_that("Error if the url is not a string", {

    url     <- 9741
    token   <- "9A81268476645C4E5F03428B8AC3AA7B"

    expect_error(read_redcap(token = token, url = url))

})

test_that("Error if the token is not given", {

    url     <- "https://bbmc.ouhsc.edu/redcap/api/"

    expect_error(read_redcap(token = token, url = url))

})

test_that("Error if the url is not given", {

    token   <- "9A81268476645C4E5F03428B8AC3AA7B"

    expect_error(read_redcap(token = token, url = url))

})

test_that("Check that study_metadata columns names are correct", {

    col_names <- c(
        "field_name", "form_name", "field_label",
        "select_choices_or_calculations", "field_note", "field_type",
        "validation_min", "validation_max", "branching_logic"
    )

    url     <- "https://bbmc.ouhsc.edu/redcap/api/"
    token   <- "9A81268476645C4E5F03428B8AC3AA7B"

    meta_data <- read_redcap(
        token = token, url = url
    )[["study_metadata"]]

    expect_identical(col_names, names(meta_data))

})

test_that("Study_metadata fields names and study_data columns names", {

    url     <- "https://bbmc.ouhsc.edu/redcap/api/"
    token   <- "9A81268476645C4E5F03428B8AC3AA7B"

    imp_list <- read_redcap(token = token, url = url)

    cols_study_data <- names(imp_list[["study_data"]])
    cols_study_metadata <- imp_list[["study_metadata"]]$field_name
    forms_study_metadata <- unique(
        imp_list[["study_metadata"]]$form_name
    )

    str_cols_study_metadata <- paste(
        "(", paste0(cols_study_metadata, collapse = ")|("), ")",
        sep = ""
    )

    str_forms_study_metadata <- paste(
        "(",
        paste0(
            c(forms_study_metadata, "redcap_data_access_group"),
            collapse = ")|("
        ),
        ")",
        sep = ""
    )

    str_eval <- paste(
        str_cols_study_metadata, str_forms_study_metadata, sep = "|"
    )

    expect_true(all(stringr::str_detect(cols_study_data, str_eval)))

})


