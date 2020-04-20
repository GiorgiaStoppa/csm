
df <- tibble::tibble(
    center = rep(c("center_1", "center_2"), each = 3L),
    id = c(
        "center_1_id_1", "center_1_id_2", "center_1_id_3",
        "center_2_id_1", "center_2_id_2", "center_2_id_3"
    ),
    birth_date = lubridate::as_date(
        c(
            "1940-01-03", "1978-01-03", "1945-04-07",
            "1968-12-05", "1979-04-25", "1984-10-10"
        )
    ),
    proc_date = lubridate::as_date(
        c(
            "2016-02-05", "2017-09-09", "2017-04-10",
            "2018-12-26", "2018-08-30", "2019-10-21"
        )
    ),
    discharge_date = lubridate::as_date(
        c(
            "2016-01-25", "2017-10-11", "2017-04-29",
            "2019-02-01", "2018-09-12", "2019-11-03"
        )
    ),
    follow_up_date = lubridate::as_date(
        c(
            "2017-02-06", "2018-09-10", "2018-04-11",
            "2019-12-27", "2018-04-25", "2020-11-04"
        )
    )
)

dates <- c(
    "birth_date", "proc_date", "discharge_date", "follow_up_date"
)

test_that("The function returns a dataframe", {
    expect_is(
        check_dates_order(
            df, dates, redcap_info = c("id", "center")
        ),
        class = "data.frame"
    )
})

test_that("Output is correct", {
    expect_equal(
        check_dates_order(
            df, dates, redcap_info = c("id", "center")
        )$tables,
        expected = c(
            "discharge_date", "proc_date", "follow_up_date",
            "proc_date", "discharge_date"
        )
    )
})
