a <- c("aa", "bb", "cc", "dd_1", "dd_2", "ff")
b <- c("a", "b", "c")
c <- c("a", "d", NA_character_)
d <- c("gg", "d_3", NA_character_)


test_that("Class of output is correct", {

    expect_is(
        remove_last_number(x = a),
        class = "character"
    )

})

test_that("Same vector if no element ends with '_' and a number", {

    expect_identical(
        remove_last_number(x = b),
        b
    )

})

test_that("Everything is ok if at least one element is NA - 1", {

    expect_identical(
        remove_last_number(x = c),
        c
    )

})

test_that("Everything is ok if at least one element is NA - 2", {

    expect_identical(
        remove_last_number(x = d),
        c("gg", "d", NA_character_)
    )

})

test_that("Error if x is not a character", {

    expect_error(remove_last_number(x = 2L))

})

test_that("Error if x is not provided", {

    expect_error(remove_last_number())

})



