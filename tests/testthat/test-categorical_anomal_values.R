
test_that("Class of output is correct", {

    x <- c("male", "female", "male", "male", "female", "x")
    values <- c("male", "female")

    expect_is(
        categorical_anomal_values(x = x, values = values),
        class = "character"
    )

})

test_that("NA if all the values are allowed", {

    x <- c("male", "female", "male", "male", "female")
    values <- c("male", "female")

    expect_equal(
        categorical_anomal_values(x = x, values = values),
        NA_character_
    )

})

test_that("Error when a numerico is passed to x", {

    x <- c(7, 4, 5)
    values <- c("male", "female")

    expect_error(continuous_anomal_values(x = x, range = range))

})

test_that("Error when a string is passed to values", {

    x <- c("male", "female", "male", "male", "female")
    values <- c(5, 6, 7)

    expect_error(continuous_anomal_values(x = x, range = range))

})

test_that("Error when no input is passed to x", {

    values <- c("male", "female")

    expect_error(continuous_anomal_values(range = range))

})

test_that("Error when no input is passed to values", {

    x <- c("male", "female", "male", "male", "female")

    expect_error(continuous_anomal_values(x = x))

})


