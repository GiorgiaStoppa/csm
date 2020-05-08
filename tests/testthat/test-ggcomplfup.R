data <- tibble::tibble(
    outcome = c("death", "death", "mi", "mi"),
    total = rep(75, 4L),
    type = c(
        "completeness", "missingness", "completeness", "missingness"
    ),
    percentage = c(65.5, 34.5, 98, 2)
)

outcome <- "outcome"
total <- "total"
type <- "type"
percentage <- "percentage"

test_that("Class of output is correct", {

    expect_is(
        ggcomplfup(data, "outcome", "total", "type", "percentage"),
        class = "gg"
    )

})

test_that("Error if some wrong variables are passed", {

    expect_error(
        ggcomplfup(data, "outcome", "total", "type", "percent"),
        class = "usethis_error",
        regexp = "Some inputs are not included in the data. Please verify that 'outcome', 'type', 'total' and 'percentage' were correctly spelled."
    )

})



