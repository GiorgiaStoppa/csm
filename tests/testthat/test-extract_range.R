md <- tibble::tibble(
    field_name = c("age", "sex", "hypertension", "diabetes"),
    sheet = c("demo", "demo", "clinical", "clinica"),
    min = c(18, NA, NA, NA),
    max = c(75, NA, NA, NA)
)

test_that("Check that the function returns a list", {
    expect_is(
        extract_range(md, "field_name", "min", "max"), class = "list"
    )
})

test_that("Check that the function returns a list with the correct length", {
    expect_equal(
        length(extract_range(md, "field_name", "min", "max")),
        expected = 1L
    )
})

test_that("Check that the function returns a list with the correct names", {
    expect_equal(
        names(extract_range(md, "field_name", "min", "max")),
        expected = "age"
    )
})

test_that("Error if some column's names are not included in the names of meta-data", {
    expect_error(
        extract_range(md, "field_name", "dd", "max"),
        class = "usethis_error",
        regexp = "'fields_names' or 'range_low' or 'range_upp' are not included in the names of the meta-data. Please make sure you provided the correct names of the columns."
    )
})

