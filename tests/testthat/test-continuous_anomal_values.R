
test_that("Class of output is correct", {

    x <- rnorm(n = 10L, mean = 10, sd = 2)
    range   <- c(6, 9)

    expect_is(
        continuous_anomal_values(x = x, range = range),
        class = "numeric"
    )

})

test_that("NA if all the values are inside the range", {

    x <- rnorm(n = 10L, mean = 10, sd = 2)
    range   <- c(2, 20)

    expect_equal(
        continuous_anomal_values(x = x, range = range),
        NA_real_
    )

})

test_that("Error when a string is passed to x", {

    x <- "aa"
    range   <- c(5, 8)

    expect_error(continuous_anomal_values(x = x, range = range))

})

test_that("Error when a string is passed to range", {

    x <- rnorm(n = 10L, mean = 10, sd = 2)
    range   <- "aa"

    expect_error(continuous_anomal_values(x = x, range = range))

})





