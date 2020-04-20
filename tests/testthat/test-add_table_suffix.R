set.seed(1)

df <- tibble::tibble(
    center = rep(c("center_1", "center_2"), each = 3L),
    id = c(
        "center_1_id_1", "center_1_id_2", "center_1_id_3",
        "center_2_id_1", "center_2_id_2", "center_2_id_3"
    ),
    age = rnorm(6, 60, 15),
    sex = sample(c("female", "male"), 6, TRUE)
)

suffix <- "demo"
except <- c("id", "center")

test_that("Class of output is correct", {

    expect_is(
        add_table_suffix(df, suffix, except),
        class = "data.frame"
    )

})

test_that("Column names are correct", {

    expect_equal(
        names(add_table_suffix(df, suffix, except)),
        expected = c("center", "id", "age_demo", "sex_demo")
    )

})

except <- c("id", "hyper")

test_that("Error if at least one element of 'except' is not included in the names of 'data'", {

    expect_error(
        add_table_suffix(df, suffix, except),
        class = "usethis_error",
        regexp = "'except' must be included in the columns names."
    )

})

suffix <- c("demo", "clinical")
except <- c("id", "center")

test_that("Error 'except' has more than one element", {

    expect_error(
        add_table_suffix(df, suffix, except),
        class = "usethis_error",
        regexp = "'except' must be a single element character."
    )

})

