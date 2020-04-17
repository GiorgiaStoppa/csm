a <- tibble::tibble(
    variable = c("aa", "dd_1", "dd_2", "dd_3", "cc")
)

b <- tibble::tibble(a = c("6", "9i"))

c <- tibble::tibble(a = c("6", "9i"), gg = c(2, 1))

d <- tibble::tibble(variable = rep(0, 4L))

test_that("Class of output is correct", {

    expect_is(tidy_fields_names(data = a), class = "data.frame")

})


test_that("Error if data is not a data.frame", {

    expect_error(tidy_fields_names(data = 2L))

})

test_that("Error if data is not a provide", {

    expect_error(tidy_fields_names())

})

test_that("Error if the name of data's column is not 'variable'", {

    expect_error(
        tidy_fields_names(b),
        class = "usethis_error",
        regexp = "The name of the column must be 'variable'"
    )

})

test_that("Error if data has more than one columns", {

    expect_error(
        tidy_fields_names(c),
        class = "usethis_error",
        regexp = "The tibble must has only one column"
    )

})

test_that("Error if the data's column is not a character", {

    expect_error(
        tidy_fields_names(d),
        class = "usethis_error",
        regexp = "The column of the tibble must be a character"
    )

})


