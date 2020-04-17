nested_tab <- tibble::tibble(
    fields = c(rep("demo_clinical", 3L), rep("follow_up", 3L)),
    sheets = c(
        "demo", "risk_factors", "clinical", "discharge",
        "month_1", "year_1"
    ),
    tables = purrr::map(
        .x = seq_len(6),
        ~ tibble::tibble(
            id = rep(glue::glue("id_{1:5}"), 2L),
            center = rep(c("center_1", "center_2"), each = 5L),
            x = c(0.1, 0.2, 0.15, 0.4, 0.5, 0.6, 0.3, 0.2, 0.2, 0.1),
            y = c(0.1, 0.11, 0.12, 0.3, 0.3, 0.3, 0.4, 0.5, 0.6, 0.55),
            z = c(0.1, 0.2, 0.15, 0.4, 0.5, 0.6, 0.3, 0.2, 0.2, 0.1)
        )
    )
)

jt <- join_tables(nested_tab, c("id", "center"))

test_that("Class of output is correct", {

    expect_is(
        jt,
        class = "data.frame"
    )

})

test_that("The number of columns is correct", {

    expect_equal(
        ncol(jt),
        expected = 2 + (6 * 3)
    )

})

test_that("First two columns are 'id' and 'center'", {

    expect_equal(
        names(jt)[1:2],
        expected = c("id", "center")
    )

})

test_that("Columns names are unique", {

    expect_equal(
        length(names(jt)),
        expected = length(unique(names(jt)))
    )

})


