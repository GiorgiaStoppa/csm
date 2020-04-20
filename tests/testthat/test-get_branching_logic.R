meta_data <- tibble::tibble(
    field_name = c(
        "id", "sex", "age", "diabetes", "iddm", "prior_pci",
        "location_prior_pci"
    ),
    sheet = c(rep("demo", 3L), rep("clinical", 4L)),
    section_header = rep(NA_character_, 7L),
    field_label = c(
        "record id", "sex", "age", "diabetes", "iddm", "prior pci",
        "location of prior pci"
    ),
    branching_logic = c(
        rep(NA_character_, 4L), "diabetes", NA_character_, "prior_pci"
    )
)

test_that("Error if 'meta_data' is not provided", {

    expect_error(
        get_branching_logic(
            fields_names = "field_name",
            branching_logic = "branching_logic"
        )
    )

})

test_that("Error if 'fields_names' is not provided", {

    expect_error(
        get_branching_logic(
            meta_data = meta_data,
            branching_logic = "branching_logic"
        )
    )

})

test_that("Error if 'branching_logic' is not provided", {

    expect_error(
        get_branching_logic(
            meta_data = meta_data,
            fields_names = "field_name"
        )
    )

})

test_that("Error if 'meta_data' is not a tibble", {

    expect_error(
        get_branching_logic(
            meta_data = "gg",
            fields_names = "field_name",
            branching_logic = "branching_logic"
        )
    )

})

test_that("Error if 'meta_data' is not a tibble", {

    expect_error(
        get_branching_logic(
            meta_data = "gg",
            fields_names = "field_name",
            branching_logic = "branching_logic"
        )
    )

})

test_that("Error if 'fields_names' is not a character", {

    expect_error(
        get_branching_logic(
            meta_data = "gg",
            fields_names = 4L,
            branching_logic = "branching_logic"
        )
    )

})

test_that("Error if 'fields_names' is not a character", {

    expect_error(
        get_branching_logic(
            meta_data = meta_data,
            fields_names = "field_name",
            branching_logic = TRUE
        )
    )

})

test_that("Check the output of the function is a character", {

    expect_is(
        get_branching_logic(
            meta_data = meta_data,
            fields_names = "field_name",
            branching_logic = "branching_logic"
        ),
        class = "character"
    )

})

test_that("Check the output of the function is named character", {

    expect_named(
        get_branching_logic(
            meta_data = meta_data,
            fields_names = "field_name",
            branching_logic = "branching_logic"
        )
    )

})


test_that("Check 'branching_logic' has no NA", {

    expect_false(
        any(
            is.na(
                get_branching_logic(
                    meta_data = meta_data,
                    fields_names = "field_name",
                    branching_logic = "branching_logic"
                )
            )
        )
    )

})
