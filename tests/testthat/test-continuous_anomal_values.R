context("test-continuous_anomal_values")

test_that("Class of output is correct", {

    x <- rnorm(n = 10L, mean = 10, sd = 2)
    range   <- c(3, 12)

    expect_is(
        continuous_anomal_values(x = x, range = range),
        class = "numeric"
    )

})
