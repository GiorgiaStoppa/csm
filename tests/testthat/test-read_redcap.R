
url     <- "https://bbmc.ouhsc.edu/redcap/api/"
token   <- "9A81268476645C4E5F03428B8AC3AA7B"
dd      <- read_redcap(token = token, url = url)

test_that("Class of output is correct", {

    expect_is(
        dd,
        class = "list"
    )

})

test_that("Class of data is ok", {

    expect_is(dd[["data"]], "list")

})

test_that("Class of meta_data is correct", {

    expect_is(dd[["meta_data"]], "list")

})

test_that("Error if the token is not a string", {


    expect_error(
        read_redcap(token = 876, url = url),
        regexp = "token is not of class 'character'"
    )

})

test_that("Error if the url is not a string", {

    expect_error(
        read_redcap(token = token, url = 9747),
        regexp = "url is not of class 'character'"
    )

})

test_that("Error if the token is not given", {

    expect_error(read_redcap(url = url))

})

test_that("Error if the url is not given", {

    expect_error(read_redcap(token = token))

})

