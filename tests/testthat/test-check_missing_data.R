
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
            z = c(
                "no", "no", "yes", NA_character_, NA_character_,
                "no", "yes", "yes", "no", NA_character_
            )
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
    branching_logic = rep(
        c(
            NA_character_, NA_character_, NA_character_,
            NA_character_, "x", NA_character_,
            "z", NA_character_, NA_character_
        ), 2L
    )
)

test_that("Check if the function returns a data.frame", {
    expect_is(
        check_missing_data(
            nested_tab, meta_data, "demo", "field_name",
            "branching_logic", redcap_info = c("id", "center")
        ),
        class = "data.frame"
    )
})

test_that("Check the function returns the right number missing fields", {
    expect_equal(
        nrow(
            check_missing_data(
                nested_tab, meta_data, "demo", "field_name",
                "branching_logic", redcap_info = c("id", "center")
            )
        ),
        expected = 6L
    )
})

test_that("Check the function returns the right missing fields", {
    expect_equal(
        check_missing_data(
            nested_tab, meta_data, "demo", "field_name",
            "branching_logic", redcap_info = c("id", "center")
        )$missing_vars,
        expected = c("z", "z", "x", "x", "y", "z")
    )
})


