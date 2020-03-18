url     <- "https://bbmc.ouhsc.edu/redcap/api/"
token   <- "9A81268476645C4E5F03428B8AC3AA7B"
data <- csm:::read_redcap(url = url, token = token)
data$data$data <- data$data$data %>%
    dplyr::mutate(redcap_event_name = "Demographics")


test_that("Class of output 'data' is correct", {

    expect_is(
        tidy_extract(data = data, type = "data"),
        class = "data.frame"
    )

})

test_that("Class of output 'meta' is correct", {

    expect_is(
        tidy_extract(data = data, type = "meta"),
        class = "data.frame"
    )

})

test_that("error if a list is not passed to 'data'", {

    expect_error(tidy_extract(data = c("a", "b"), type = "meta"))

})

test_that("error if a character is not passed to 'type'", {

    expect_error(tidy_extract(data = data, type = 5L))

})

test_that("error if nothing is passed to 'data'", {

    expect_error(tidy_extract(type = "data"))

})

test_that("error if nothing is something different from 'data' or 'meta is passed to 'type'", {

    expect_error(tidy_extract(data = data, type = "ff"))

})

