
test_that("Error if a string is not passed", {

    expect_error(str_to_level(x = 4))

})

test_that("Error if a empty string is passed", {

    expect_error(str_to_level(x = ""), regexp = "x has no characters")

})

test_that("Class of the output is correct", {

    expect_is(
        str_to_level(x = "1, maschio | 2, femmina | 3, ambiguo"),
        "character"
    )

})


