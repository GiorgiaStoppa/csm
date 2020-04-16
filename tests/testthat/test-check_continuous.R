
range <- list("age" = c(18, 70), "hemoglobin" = c(6, 18))
df <- tibble::tibble(age = 16, hemoglobin = 9)

test_that("Class of output is correct", {

    expect_is(
        check_continuous(data = df, range = range),
        class = "character"
    )

})

test_that("Output as expected", {

    expect_equal(
        check_continuous(data = df, range = range),
        expected = "age"
    )

})

range <- list("age" = c(18, 70), "hemoglobin" = c(6, 18))
df <- tibble::tibble(age = 19, hemoglobin = 9)

test_that("'character(0)' if no continuous variable has out-of-range value", {

    expect_equal(
        check_continuous(data = df, range = range),
        expected = character(0)
    )

})

range <- list("age" = c(18, 70), "hemoglobin" = c(6, 18))
df <- tibble::tibble(sex = 19, hemoglobin = 9)

test_that("Error if names of range and names of data are different", {

    expect_error(
        check_continuous(data = df, range = range),
        class = "usethis_error",
        regexp = "Names of the data and names of the range are different. Please make sure that you correctly specify the columns to which checking must be performed."
    )

})

