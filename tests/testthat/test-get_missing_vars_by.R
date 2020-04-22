df_1 <- tibble::tibble(
    id = c("id_1", "id_2", "id_3"),
    age = c(63, 65, 66),
    sex = c("female", NA_character_, "male"),
    diabetes = c(NA_character_, "yes", "no"),
    hypertension = c("yes", NA_character_, NA_character_),
    nyha = rep(NA_character_, 3L)
)

df_2 <- tibble::tibble(
    id = c("id_1", "id_2", "id_3"),
    age = rep(NA_character_, 3L),
    sex = rep(NA_character_, 3L),
    diabetes = rep(NA_character_, 3L),
    hypertension = rep(NA_character_, 3L),
    nyha = rep(NA_character_, 3L)
)

df_3 <- tibble::tibble(
    id = c("id_1", "id_2", "id_3"),
    age = c(55, 54, 67),
    sex = c("female", "female", "male"),
    diabetes = c("no", "yes", "yes"),
    hypertension = c("yes", "no", "no"),
    nyha = c("III", "II", "II")
)


test_that("Check if the function returns a data.frame", {
    expect_is(
        get_missing_vars_by(df_1, "id"),
        class = "data.frame"
    )
})

test_that("Check the output is as expected", {
    expect_equal(
        get_missing_vars_by(df_1, "id"),
        expected = tibble::tibble(
            id = c(
                "id_1", "id_1", "id_2", "id_2", "id_2", "id_3", "id_3"
            ),
            missing_vars = c(
                "diabetes", "nyha", "sex", "hypertension", "nyha",
                "hypertension", "nyha"
            )
        )
    )
})

test_that("Check the output is as expected", {
    expect_equal(
        get_missing_vars_by(df_2, "id"),
        expected = tibble::tibble(
            id = rep(c("id_1", "id_2", "id_3"), each = 5L),
            missing_vars = rep(
                c("age", "sex", "diabetes", "hypertension", "nyha"),
                3L
            )
        )
    )
})

test_that("Check the output is as expected", {
    expect_equal(
        get_missing_vars_by(df_3, "id"),
        expected = tibble::tibble(
            id = character(), missing_vars = character()
        )
    )
})


