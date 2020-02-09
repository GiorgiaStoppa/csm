context("test-import_redcap")

url     <- "https://bbmc.ouhsc.edu/redcap/api/"
token   <- "9A81268476645C4E5F03428B8AC3AA7B"

test_that("Class of output is correct", {

    expect_is(
        import_redcap(token = token, url = url),
        class = "list"
    )

})

test_that("Class of the imported data is ok", {

    expect_is(
        import_redcap(token = token, url = url)[["data"]],
        class = "data.frame"
    )

})

test_that("Class of the metadata is ok", {

    expect_is(
        import_redcap(token = token, url = url)[["meta_data"]],
        class = "data.frame"
    )

})

test_that("Error if the token is not a string", {

    expect_error(
        import_redcap(token = 876, url = url),
        regexp = "token is not of class 'character'"
    )

})

test_that("Error if the url is not a string", {

    expect_error(
        import_redcap(token = token, url = 9741),
        "url is not of class 'character'"
    )

})


