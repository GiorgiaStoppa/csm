nested_tab <- tibble::tibble(
    field = c(rep("demo_clinical", 3L), rep("follow_up", 3L)),
    sheet = c(
        "demo", "risk_factors", "clinical", "discharge",
        "month_1", "year_1"
    ),
    table = purrr::map(
        .x = seq_len(6),
        ~ tibble::tibble(x = runif(10), y = rnorm(10))
    )
)

test_that("Error if 'x' is not provided", {

    expect_error(get_sheet(sheet = "demo", field = "demo_clinical"))

})

test_that("Error if 'sheet' is not provided", {

    expect_error(get_sheet(x = nested_tab, field = "demo_clinical"))

})

test_that("Error if 'x' is not data.frame", {

    expect_error(
        get_sheet(
            x = c("a", "b"), sheet = "demo", field = "demo_clinical"
        )
    )

})

test_that("Error if 'sheet' is not a character", {

    expect_error(
        get_sheet(
            x = nested_tab, sheet = 4L, field = "demo_clinical"
        )
    )

})

test_that("Error if 'field' is not a character", {

    expect_error(
        get_sheet(
            x = nested_tab, sheet = "demo", field = c(8, 9)
        ),
        regexp = "If the field is not NULL, it must be a character"
    )

})

test_that("Error if 'field' is not a character", {

    expect_error(
        get_sheet(
            x = nested_tab, sheet = "demo", field = c(8, 9)
        ),
        regexp = "If the field is not NULL, it must be a character"
    )

})

test_that("Error if a sheet which does not exist is passed", {

    expect_error(
        get_sheet(
            x = nested_tab, sheet = "levels", field = "demo_clinical"
        )
    )

})

