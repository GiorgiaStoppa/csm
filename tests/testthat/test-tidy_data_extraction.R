dd <- tibble::tibble(
    id = c(rep("1", 2L), rep("2", 2L), rep("3", 2L)),
    redcap_event_name = rep(c("demo and clinical", "discharge"), 3L),
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


test_that("Class of output is correct", {

    expect_is(
        tidy_data_extraction(data = study_data),
        class = "data.frame"
    )

})

test_that("`tidy_data_extraction` returns error if a list is not passed as input", {

    expect_error(tidy_data_extraction(data = dd))

})

test_that("`tidy_data_extraction` returns error if a inputs is not passed", {

    expect_error(tidy_data_extraction())

})

