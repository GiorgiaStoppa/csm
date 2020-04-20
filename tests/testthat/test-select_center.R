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

test_that("Class of output is correct", {

    expect_is(
        select_center(
            nested_tab, center_id = "center", center_label = "center_2"
        ),
        class = "data.frame"
    )

})

test_that("Right number of rows in each table", {

    expect_equal(
        purrr::map_dbl(
            .x = select_center(
                nested_tab, center_id = "center",
                center_label = "center_2"
            )$tables,
            ~ nrow(.x)
        ),
        expected = rep(5, 6L)
    )

})

test_that("Only the data from 'center_2' are returned", {

    expect_equal(
        purrr::map_dfr(
            .x = select_center(
                nested_tab, center_id = "center",
                center_label = "center_2"
            )$tables,
            ~ .x %>%
                dplyr::select(.data[["center"]])
        ),
        expected = tibble::tibble(center = rep("center_2", 6 * 5))
    )

})

