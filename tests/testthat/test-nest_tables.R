dd <- tibble::tibble(
    id = c(rep("1", 2L), rep("2", 2L), rep("3", 2L)),
    fields = rep(c("demo and clinical", "discharge"), 3L),
    sex = c(
        "female", NA_character_,
        "male", NA_character_,
        "female", NA_character_
    ),
    age = c(
        44, NA_real_,
        50, NA_real_,
        71, NA_real_
    ),
    demo_complete = c(
        "complete", NA_character_,
        "complete", NA_character_,
        "complete", NA_character_
    ),
    hp = c(
        "yes", NA_character_,
        "no", NA_character_,
        "yes", NA_character_
    ),
    diabetes = c(
        "no", NA_character_,
        "no", NA_character_,
        "yes", NA_character_
    ),
    clinical_complete = c(
        "complete", NA_character_,
        "complete", NA_character_,
        "complete", NA_character_
    ),
    fe = c(
        NA_real_, 35,
        NA_real_, 56,
        NA_real_, 44
    ),
    death = c(
        NA_character_, "no",
        NA_character_, "no",
        NA_character_, "yes"
    ),
    discharge_complete = c(
        NA_character_, "complete",
        NA_character_, "complete",
        NA_character_, "complete"
    )
)

md <- tibble::tibble(
    field_name = c("sex", "age", "hp", "diabetes", "fe", "death"),
    form_name = c(rep("demo and clinical", 4L), rep("discharge", 2L))
)

study_data <- list("data" = dd, "meta_data" = md)

test_that("`sheets_to_var` returns error if a list is not passed to `data`", {

    expect_error(
        sheets_to_var(data = study_data, name = "sheets", exept = "id")
    )

})

test_that("`sheets_to_var` eturns error if a character is not passed to `study_data`", {

    expect_error(
        sheets_to_var(data = dd, name = study_data, exept = "id")
    )

})

test_that("`sheets_to_var` eturns error if a character is not passed to `exept`", {

    expect_error(
        sheets_to_var(data = dd, name = "sheets", exept = study_data)
    )

})

test_that("`sheets_to_var` returns error if nothing is passed to `data`", {

    expect_error(sheets_to_var(name = "sheets", exept = "id"))

})

test_that("`sheets_to_var` returns error if nothing is passed to `study_data`", {

    expect_error(sheets_to_var(data = dd, exept = "id"))

})

test_that("`sheets_to_var` returns error if nothing is passed to `exept`", {

    expect_error(sheets_to_var(data = dd, name = "sheets"))

})

test_that("Class of `sheets_to_var` output is correct", {

    expect_is(
        sheets_to_var(data = dd, name = "sheets", exept = "id"),
        class = "data.frame"
    )

})

test_that("Class of `nest_tables` output is correct", {

    expect_is(
        nest_tables(data = dd, id = "id"),
        class = "data.frame"
    )

})

test_that("`nest_tables` returns error if something different from a
          data.frame is passed to `data`", {

    expect_error(nest_tables(data = study_data, id = "id"))

})

test_that("`nest_tables` returns error if something different from a
          character is passed to `id`", {

    expect_error(nest_tables(data = dd, id = study_data))

})

test_that("`nest_tables` returns error if nothing is passed to `data`", {

    expect_error(nest_tables(id = "id"))

})

test_that("`nest_tables` returns error if nothing is passed to `id`", {

    expect_error(nest_tables(data = dd))

})
