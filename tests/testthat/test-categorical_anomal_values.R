x <- c("male", "female", "male", "male", "female", "x")
values <- c("male", "female")

test_that("Class of output is correct", {

    expect_is(
        categorical_anomal_values(x = x, values = values),
        class = "character"
    )

})

x <- c("male", "female", "male", "male", "female")

test_that("NA if all the values are allowed", {

    expect_equal(
        categorical_anomal_values(x = x, values = values),
        character(0)
    )

})

x <- c(7, 4, 5)

test_that("Error when a numeric is passed to x", {


    expect_error(
        categorical_anomal_values(x = x, values = values),
        regexp = "x is not of class 'character'; it has class 'numeric'."
    )

})

x <- c("male", "female", "male", "male", "female")
values <- c(5, 6, 7)

test_that("Error when a string is passed to values", {

    expect_error(
        categorical_anomal_values(x = x, values = values),
        regexp = "values is not of class 'character'; it has class 'numeric'."
    )

})

values <- c("male", "female")

test_that("Error when no input is passed to x", {

    expect_error(
        categorical_anomal_values(values = values),
        regexp = "argument \"x\" is missing, with no default"
    )

})

test_that("Error when no input is passed to values", {

    expect_error(
        categorical_anomal_values(x = x),
        regexp = "argument \"values\" is missing, with no default"
    )

})


