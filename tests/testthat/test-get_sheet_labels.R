df <- tibble::tibble(
    id = rep(glue::glue("id_{1:5}"), 2L),
    center = rep(c("center_1", "center_2"), each = 5L),
    x = c(0.1, 0.2, 0.15, 0.4, 0.5, 0.6, 0.3, 0.2, 0.2, 0.1),
    y = c(0.1, 0.11, 0.12, 0.3, 0.3, 0.3, 0.4, 0.5, 0.6, 0.55),
    z = c(0.1, 0.2, 0.15, 0.4, 0.5, 0.6, 0.3, 0.2, 0.2, 0.1),
    complete = rep("complete", 10L)
)

meta_data <- tibble::tibble(
    field_name = c("age", "sex", "diabetes", "x", "y", "z"),
    sheet = c("demo", "demo", "clinical", "random", "random", "random"),
    field_label = c(
        "age", "sex", "diabetes", "first tt", "second tt", "third tt"
    )
)

redcap_info <- c("id", "center")
fields_labels <- "field_label"
fields_names <- "field_name"

get_sheet_labels(
    data = df, meta_data = meta_data,
    redcap_info = redcap_info, fields_names = fields_names,
    fields_labels = fields_labels
)


test_that("Check if the function returns a character", {
    expect_is(
        get_sheet_labels(
            data = df, meta_data = meta_data,
            redcap_info = redcap_info, fields_names = fields_names,
            fields_labels = fields_labels
        ),
        class = "character"
    )
})

test_that("Check if the function returns a characters with names", {
    expect_length(
        names(get_sheet_labels(
            data = df, meta_data = meta_data,
            redcap_info = redcap_info, fields_names = fields_names,
            fields_labels = fields_labels
        )),
        n = 3
    )
})

test_that("Check the output is as expected", {
    expect_equal(
        get_sheet_labels(
            data = df, meta_data = meta_data,
            redcap_info = redcap_info, fields_names = fields_names,
            fields_labels = fields_labels
        ),
        expected = c("First Tt", "Second Tt", "Third Tt") %>%
            purrr::set_names(c("x", "y", "z"))
    )
})





