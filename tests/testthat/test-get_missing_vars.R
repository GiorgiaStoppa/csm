df_1 <- tibble::tibble(
    age = 63,
    sex = "female",
    diabetes = NA_character_,
    hypertension = "yes",
    nyha = NA_character_
)

df_2 <- tibble::tibble(
    age = NA_character_,
    sex = NA_character_,
    diabetes = NA_character_,
    hypertension = NA_character_,
    nyha = NA_character_
)

df_3 <- tibble::tibble(
    age = 55,
    sex = "female",
    diabetes = "no",
    hypertension = "yes",
    nyha = "III"
)


test_that("Check if the function returns a character", {
    expect_is(
        get_missing_vars(df_1),
        class = "character"
    )
})

test_that("Check the function returns the correct missing fields", {
    expect_equal(
        get_missing_vars(df_1),
        expected = c("diabetes", "nyha")
    )
})

test_that("Check the function returns all the fields if all fields are missing", {
    expect_equal(
        get_missing_vars(df_2),
        expected = names(df_2)
    )
})

test_that("Check the function returns `character(0)` if no field is missing", {
    expect_equal(
        get_missing_vars(df_3),
        expected = character(0)
    )
})


