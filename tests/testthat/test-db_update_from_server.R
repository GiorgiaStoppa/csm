
test_that("Error if 'token' is not provided", {

    expect_error(
        db_update_from_server(
            path_data = data_path(),
            redcap_info = c("id", "site"),
            file_name = "tidy_data.rds"
        )
    )

})

test_that("Error if 'token' is not a string", {

    expect_error(
        db_update_from_server(
            token = 4,
            path_data = data_path(),
            redcap_info = c("id", "site"),
            file_name = "tidy_data.rds"
        )
    )

})

test_that("Error if 'redcap_info' is not a string", {

    expect_error(
        db_update_from_server(
            token = "abs",
            path_data = data_path(),
            redcap_info = TRUE,
            file_name = "tidy_data.rds"
        )
    )

})

test_that("Error if 'file_name' is not a string", {

    expect_error(
        db_update_from_server(
            token = "abs",
            path_data = data_path(),
            redcap_info = c("id", "site"),
            file_name = 4L
        )
    )

})

test_that("Error if 'file_name' is not rds", {

    expect_error(
        db_update_from_server(
            token = "abs",
            path_data = data_path(),
            redcap_info = c("id", "site"),
            file_name = "tidy_data.rda"
        ),
        class = "usethis_error"
    )

})

