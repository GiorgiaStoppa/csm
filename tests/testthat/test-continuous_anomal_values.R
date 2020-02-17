
x <- rnorm(n = 10L, mean = 10, sd = 2)
range   <- c(6, 9)

test_that("Class of output is correct", {

    expect_is(
        continuous_anomal_values(x = x, range = range),
        class = "numeric"
    )

})

range   <- c(2, 20)

test_that("NA if all the values are inside the range", {

    expect_equal(
        continuous_anomal_values(x = x, range = range),
        numeric(0)
    )

})

x <- "aa"
range   <- c(5, 8)

test_that("Error when a string is passed to x", {

    expect_error(
        continuous_anomal_values(x = x, range = range),
        regexp = "x is not of class 'numeric'; it has class 'character'."
    )

})

x <- rnorm(n = 10L, mean = 10, sd = 2)
range   <- "aa"

test_that("Error when a string is passed to range", {

    expect_error(
        continuous_anomal_values(x = x, range = range),
        regexp = "range is not of class 'numeric'; it has class 'character'."
    )

})

range <- c(5, 8)

test_that("Error when x is not passed", {

    expect_error(
        continuous_anomal_values(range = range),
        regexp = "argument \"x\" is missing, with no default"
    )

})

x <- rnorm(n = 10L, mean = 10, sd = 2)

test_that("Error when range is not passed", {

    expect_error(
        continuous_anomal_values(x = x),
        regexp = "argument \"range\" is missing, with no default"
    )


})

range <- c(5, 3)

test_that("Warning when max value is less than min value", {

    expect_warning(
        continuous_anomal_values(x = x, range = range),
        regexp = "Max in range is below the min. They will be switched."
    )


})


