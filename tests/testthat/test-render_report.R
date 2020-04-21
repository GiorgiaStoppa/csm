
token <- "aa"
path_data <- "data"
redcap_info <- c("id", "center")
file_name <- "tidy_data.rds"
center_names <- c("center_1", "center_2", "center_3")
center_labels <- c("Center 1", "Center 2", "Center 3")
path_template <- c("report/template.rrr")

test_that("Error if the template is not an '.Rmd'", {

    expect_error(
        render_report(
            token = token,
            path_data = path_data,
            redcap_info = redcap_info,
            file_name = file_name,
            center_names = center_names,
            center_labels = center_labels,
            path_template = path_template
        ),
        class = "usethis_error",
        regexp = "The template must be a '.Rmd' file"
    )

})

center_labels <- c("Center 1", "Center 2", "Center 3")
path_template <- c("report/template.Rmd")
file_name <- "tidy_data.rrr"

test_that("Error if 'file_name' does not have a .rds format", {

    expect_error(
        render_report(
            token = token,
            path_data = path_data,
            redcap_info = redcap_info,
            file_name = file_name,
            center_names = center_names,
            center_labels = center_labels,
            path_template = path_template
        ),
        class = "usethis_error",
        regexp =  "The data must be stored in a '.rds' file."
    )

})

path_template <- c("report/template.rda")
file_name <- "tidy_data.rds"

test_that("Error if the template is not a .Rmd file", {

    expect_error(
        render_report(
            token = token,
            path_data = path_data,
            redcap_info = redcap_info,
            file_name = file_name,
            center_names = center_names,
            center_labels = center_labels,
            path_template = path_template
        ),
        class = "usethis_error",
        regexp =   "The template must be a '.Rmd' file"
    )

})

