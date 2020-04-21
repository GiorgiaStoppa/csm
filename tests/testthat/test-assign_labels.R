x <- c("a", "b", "c")
y <- c("1", "2", "3") %>%
    purrr::set_names(nm = c("a", "b", "c"))

test_that("Check if the function returns a character", {
    expect_is(
        assign_labels(x, y),
        class = "character"
    )
})

test_that("Check the output is as expected", {
    expect_equal(
        assign_labels(x, y),
        expected = c("1", "2", "3")
    )
})

x <- c("a", "b")

test_that("Check the output is as expected", {
    expect_equal(
        assign_labels(x, y),
        expected = c("1", "2")
    )
})

x <- c("a", "d")

test_that("Expect error if 'x' contains at least one elements that is not included in th names of 'y'", {
    expect_error(
        assign_labels(x, y),
        class = "usethis_error",
        regexp = "All the elements of 'x' must be included in the names of 'y'"
    )
})
