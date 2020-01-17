context("test-import_redcap")

test_that("Class of output is correct", {

    url     <- "https://bbmc.ouhsc.edu/redcap/api/"
    token   <- "9A81268476645C4E5F03428B8AC3AA7B"

    expect_is(
        import_redcap(token = token, url = url),
        class = "list"
    )

})

test_that("Class of the imported data is ok", {

    url     <- "https://bbmc.ouhsc.edu/redcap/api/"
    token   <- "9A81268476645C4E5F03428B8AC3AA7B"

    expect_is(
        import_redcap(token = token, url = url)[["data"]],
        class = "data.frame"
    )

})

test_that("Class of the metadata is ok", {

    url     <- "https://bbmc.ouhsc.edu/redcap/api/"
    token   <- "9A81268476645C4E5F03428B8AC3AA7B"

    expect_is(
        import_redcap(token = token, url = url)[["meta_data"]],
        class = "data.frame"
    )

})

test_that("Error if the token is not a string", {

    url     <- "https://bbmc.ouhsc.edu/redcap/api/"
    token   <- 876

    expect_error(import_redcap(token = token, url = url))

})

test_that("Error if the url is not a string", {

    url     <- 9741
    token   <- "9A81268476645C4E5F03428B8AC3AA7B"

    expect_error(import_redcap(token = token, url = url))

})


