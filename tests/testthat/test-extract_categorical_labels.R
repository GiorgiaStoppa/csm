md <- tibble::tibble(
    field_name = c("age", "sex", "hypertension", "diabetes"),
    sheet = c("demo", "demo", "clinical", "clinica"),
    fct_level = list(
        character(0), c("female", "male"), c("yes", "no"),
        c("yes", "no")
    )
)

mf <- tibble::tibble(
    field_name = c("age", "sex", "hypertension", "diabetes"),
    sheet = c("demo", "demo", "clinical", "clinica"),
    fct_level = c(3, 4, 5, 6)
)

tt <- extract_categorical_labels(md, "field_name", "fct_level")


test_that("Check that the function returns a list", {
    expect_is(tt, class = "list")
})

test_that("Check that the names of the list are correct", {
    expect_equal(
        names(tt), expected = c("sex", "hypertension", "diabetes")
    )
})

test_that("Check that the list doesn't contain elements that are not character", {
    expect_true(
        all(
            purrr::map_lgl(.x = tt, ~ is.character(.x))
        )
    )
})

test_that("Check that the all the elements of the list do not have lenght 0", {
    expect_true(
        all(
            purrr::map_lgl(.x = tt, ~ length(.x) != 0)
        )
    )
})

test_that("Error if `fields_names` or `labels` are not included in the names of `meta_data`", {
    expect_error(
        extract_categorical_labels(md, "ff", "labels"),
        class = "usethis_error",
        regexp = "'fields_names' or 'labels' are not included in the names of the meta-data. Please make sure you provided the correct names of the columns."
    )
})

test_that("Error if `labels` is not a list", {
    expect_error(
        extract_categorical_labels(mf, "field_name", "labels"),
        class = "usethis_error",
        regexp = "The column with the labels of the fields must be a list. Please make sure that you provided the correct name of the column with the labels of the fields."
    )
})
