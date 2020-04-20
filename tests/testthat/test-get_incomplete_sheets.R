
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
            x = c(runif(8), rep(NA, 2L)),
            y = c(runif(9), NA),
            complete = dplyr::if_else(
                is.na(x) | is.na(y), "incomplete", "complete"
            )
        )
    )
)


test_that("Check if the function returns a data.frame", {
    expect_is(
        get_incomplete_sheets(
            nested_tables = nested_tab,
            redcap_info = c("id", "center")
        ),
        class = "data.frame"
    )
})

test_that("Check the function provides the patients with incomplete sheet", {
    expect_equal(
        nrow(
            get_incomplete_sheets(
                nested_tables = nested_tab,
                redcap_info = c("id", "center")
            )
        ),
        expected = 12
    )
})

test_that("Check the sheet column contains only the name of the sheet provided", {
    expect_equal(
        get_incomplete_sheets(
            nested_tables = nested_tab,
            redcap_info = c("id", "center")
        )$sheet,
        expected = rep(
            c(
                "demo", "risk_factors", "clinical", "discharge",
                "month_1", "year_1"
            ), each = 2L
        )
    )
})
