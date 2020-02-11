context("test-import_from_redcap")

url     <- "https://bbmc.ouhsc.edu/redcap/api/"
token   <- "9A81268476645C4E5F03428B8AC3AA7B"

test_that("Class of output is correct", {

    expect_is(
        read_redcap(token = token, url = url),
        class = "list"
    )

})

test_that("Class of study_data is ok", {

    data <- read_redcap(token = token, url = url)[["study_data"]]

    data %>% expect_is("data.frame")
    unique(names(data)) %>% expect_length(length(data))

})

test_that("Class of the study_metadata is correct", {

    data <- read_redcap(token = token, url = url)[["study_metadata"]]

    data %>% expect_is("data.frame")
    unique(names(data)) %>% expect_length(length(data))

})

test_that("Error if the token is not a string", {


    expect_error(
        read_redcap(token = 876, url = url),
        regexp = "token is not of class 'character'"
    )

})

test_that("Error if the url is not a string", {

    expect_error(
        read_redcap(token = token, url = 9747),
        regexp = "url is not of class 'character'"
    )

})

test_that("Error if the token is not given", {

    expect_error(read_redcap(url = url))

})

test_that("Error if the url is not given", {

    expect_error(read_redcap(token = token))

})

test_that("Check that study_metadata columns names are correct", {

    col_names <- c(
        "field_name", "form_name", "field_label",
        "select_choices_or_calculations", "field_note", "field_type",
        "validation_min", "validation_max", "branching_logic"
    )

    meta_data <- read_redcap(
        token = token, url = url
    )[["study_metadata"]]

    expect_identical(col_names, names(meta_data))

})

test_that("Study_metadata fields names and study_data columns names", {

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


