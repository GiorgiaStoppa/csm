df <- tibble(
    foo = c("a", "a", "b"),
    bar = 1:3,
    baz = c("one", "two", "three"),
    qux = c("random", "sample", "text")
)

fct_levels <- list(
    foo = c("a", "b", "c", "d"),
    baz = c("one", "two", "three")
)

test_that("Error if 'x' takes a object different from a data.frame", {

    expect_error(make_factors(x = c(4, 5), lev = fct_levels))

})

test_that("Error if 'lev' takes a object different from a list", {

    expect_error(make_factors(x = df, lev = c("a", "b")))

})

test_that("Error if 'x' is not passed", {

    expect_error(make_factors(lev = fct_levels))

})

test_that("Error if 'lev' is not passed", {

    expect_error(make_factors(x = df))

})
