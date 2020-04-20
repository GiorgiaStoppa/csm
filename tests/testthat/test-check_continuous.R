
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

meta_data <- tibble::tibble(
    field_name = rep(c("x", "y", "z"), 6L),
    sheet = rep(
        c(
            "demo", "risk_factors", "clinical", "discharge",
            "month_1", "year_1"
        ), each = 3L
    ),
    min = rep(c("0", "0", "0.1"), 6L),
    max = rep(c("0.5", "0.5", "0.8"), 6L)
)

test_that("Check if the function returns a data.frame", {
    expect_is(
        check_continuous(
            nested_tables = nested_tab,
            meta_data = meta_data,
            redcap_info = c("id", "center"),
            tab = "demo",
            fields_names = "field_name",
            range_low = "min",
            range_upp = "max"
        ),
        class = "data.frame"
    )
})

test_that("Check the correct variables are returned", {
    expect_equal(
        check_continuous(
            nested_tables = nested_tab,
            meta_data = meta_data,
            redcap_info = c("id", "center"),
            tab = "demo",
            fields_names = "field_name",
            range_low = "min",
            range_upp = "max"
        )$variables,
        expected = c("x", "y", "y")
    )
})

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
            x = runif(10, 0, 0.5),
            y = runif(10, 0, 0.5),
            z = runif(10, 0.1, 0.8)
        )
    )
)

test_that("Check all the values are within the range", {
    expect_equal(
        nrow(
            check_continuous(
                nested_tables = nested_tab,
                meta_data = meta_data,
                redcap_info = c("id", "center"),
                tab = "demo",
                fields_names = "field_name",
                range_low = "min",
                range_upp = "max"
            )
        ),
        expected = 0
    )
})
