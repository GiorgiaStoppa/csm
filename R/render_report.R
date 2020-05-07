#' Render a report for each center
#'
#' The function renders a report with the data quality check for
#' each center. A template (which must be a .Rmd file) and a list of
#' centers must be provided.
#'
#' @param token (chr) a character string that specifies the token to
#'                    get access to the data of the study. This will
#'                    be passed to [db_update_from_server].
#'
#' @param path_data (chr) path for data folder (default is `data/`
#'                        under the current project or the
#'                        [here][here::here] path. If the folder does
#'                        not exist, it will be created). This will
#'                        be passed to [db_update_from_server].
#'
#' @param redcap_info (character) the names of the variables that
#'                                identify the subject and the site
#'                                where the subject was enrolled. This
#'                                will be passed to
#'                                [db_update_from_server].
#'
#' @param file_name (chr) file name in which data are stored. This will
#'                        be passed to [db_update_from_server].
#'
#' @param center_names (chr) a character vector whose elements are the
#'                           names of the center as labelled in the
#'                           data.
#' @param center_labels (chr) a character vector whose elements are the
#'                            names that will be given to the html
#'                            file containing the report.
#' @param path_template (chr) the path and the name of the files that
#'                            contains the template for the report.
#' @param pb (lgl) a logical that indicates if a progress bar should
#'                 be displayed. Default is FALSE.
#' @param ... additional inputs that must be passed to
#'            [render][rmarkdown::render].
#'
#' @details The function aims to automate the implementation of
#'          data quality report for each center involved in the study.
#'          It works as follows:
#'            - An updated version of the dataset is retrieved from the
#'              server
#'            - A list of nested_tables (see [nest_tables]) containing
#'              the data for each center is created
#'            - A report file is rendered for each element of the list
#'              created in the previous step. One html file will be
#'              created for each element of the list.
#'          __Note__: in the template, the object that contains the
#'          nested table of the center must be named as `db`. Moreover,
#'          the object that ontains the meta-data of the study must
#'          be names as `meta_data` in the template.
#'
#' @export
#'
#' @examples
#'
#'\dontrun{
#'render_report()
#'}
#'

render_report <- function(
    token,
    path_data = data_path(),
    redcap_info = c("record_id", "center"),
    file_name = "tidy_data.rds",
    center_names,
    center_labels,
    path_template,
    pb = FALSE,
    ...
) {

    assertive::assert_is_character(token)
    assertive::assert_is_character(path_data)
    assertive::assert_is_character(redcap_info)
    assertive::assert_is_character(file_name)
    assertive::assert_is_character(center_labels)
    assertive::assert_is_character(center_names)
    assertive::assert_is_character(path_template)
    assertive::assert_is_logical(pb)

    if(length(center_names) != length(center_labels)) {
        usethis::ui_stop(
            "The vectors 'center_labels' and 'center_names' must have the same length."
        )
    }

    if(stringr::str_ends(path_template, "Rmd", negate = TRUE)) {
        usethis::ui_stop(
            "The template must be a '.Rmd' file"
        )
    }

    # Retrive id of patients and centers -------------------------------
    id_pat <- redcap_info[1]
    id_center <- redcap_info[2]

    # Get the nested data and meta-data --------------------------------
    dd <- read_redcap(url = study_redcap_url(), token = token)

    up_db <- tidy_extract(dd, "data") %>%
        nest_tables(redcap_info = redcap_info)

    md <- tidy_extract(dd, "meta")

    # Create a nested dataframe for each center ------------------------
    nested_list <- purrr::map(
        .x = center_names,
        ~ up_db %>%
            select_center(center_id = id_center, center_label = .x)
    ) %>%
        purrr::set_names(nm = center_labels)

    # Render one html for each center ----------------------------------
    if (pb) {

        progress_bar <- dplyr::progress_estimated(length(nested_list))

        invisible(
            purrr::imap(
                .x = nested_list,
                ~ {

                    db <- .x

                    meta_data <- md

                    rmarkdown::render(
                        input = here::here(path_template),
                        output_file = glue::glue(
                            "{.y}.html"
                        ),
                        quiet = TRUE
                    )

                    progress_bar$tick()$print()

                }
            )
        )

    } else {

        invisible(
            purrr::imap(
                .x = nested_list,
                ~ {

                    db <- .x

                    meta_data <- md

                    rmarkdown::render(
                        input = here::here(path_template),
                        output_file = glue::glue(
                            "{.y}.html"
                        ),
                        quiet = TRUE
                    )

                }
            )
        )

    }
}
