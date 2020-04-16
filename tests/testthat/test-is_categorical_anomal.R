x <- c("male", "female", "male", "male", "female", "x")
values <- c("male", "female")

test_that("Class of output is correct", {

    expect_is(
        is_categorical_anomal(x = x, values = values),
        class = "logical"
    )

})

x <- c("male", "female", "male", "male", "female")

test_that("FALSE if all the values are allowed", {

    expect_false(
        all(is_categorical_anomal(x = x, values = values))
    )

})

x <- c(7, 4, 5)

test_that("Error when a numeric is passed to x", {


    expect_error(
        is_categorical_anomal(x = x, values = values),
        regexp = "x is not of class 'character'; it has class 'numeric'."
    )

})

x <- c("male", "female", "male", "male", "female")
values <- c(5, 6, 7)

test_that("Error when a string is passed to values", {

    expect_error(
        is_categorical_anomal(x = x, values = values),
        regexp = "values is not of class 'character'; it has class 'numeric'."
    )

})

values <- c("male", "female")

test_that("Error when no input is passed to x", {

    expect_error(
        is_categorical_anomal(values = values),
        regexp = "argument \"x\" is missing, with no default"
    )

})

test_that("Error when no input is passed to values", {

    expect_error(
        is_categorical_anomal(x = x),
        regexp = "argument \"values\" is missing, with no default"
    )

})


