#' Get a the labels associated to each field of a given sheet
#'
#' The function returns the labels associated to each field of a given
#' sheet.
#'
#' @param data (data.frame) output from [get_sheet].
#' @param meta_data (data.frame) the data.frame the contains the
#'                               meta-data of the study.
#' @param redcap_info (character) the names of the variables that
#'                                identify the subject and the site
#'                                where the subject was enrolled.
#' @param fields_names (character) a character with the name of the
#'                                column of `meta_data` that contains
#'                                the names of fields.
#' @param fields_labels (character) a character with the name of the
#'                                  column of `meta_data` that contains
#'                                  the labels of fields.
#' @return a (character) vector whose elements are the labels associated
#'         to the fields of the sheet. The vector must be named and the
#'         names correspond to the names of the fields.
#' @export
#'
#' @examples
#' df <- tibble::tibble(
#'     id = rep(glue::glue("id_{1:5}"), 2L),
#'     center = rep(c("center_1", "center_2"), each = 5L),
#'     x = c(0.1, 0.2, 0.15, 0.4, 0.5, 0.6, 0.3, 0.2, 0.2, 0.1),
#'     y = c(0.1, 0.11, 0.12, 0.3, 0.3, 0.3, 0.4, 0.5, 0.6, 0.55),
#'     z = c(0.1, 0.2, 0.15, 0.4, 0.5, 0.6, 0.3, 0.2, 0.2, 0.1),
#'     complete = rep("complete", 10L)
#' )
#'
#' meta_data <- tibble::tibble(
#'     field_name = c("age", "sex", "diabetes", "x", "y", "z"),
#'     sheet = c("demo", "demo", "clinical", "random", "random", "random"),
#'     field_label = c(
#'         "age", "sex", "diabetes", "first tt", "second tt", "third tt"
#'     )
#' )
#'
#' redcap_info <- c("id", "center")
#' fields_labels <- "field_label"
#' fields_names <- "field_name"
#'
#' get_sheet_labels(
#'     data = df, meta_data = meta_data,
#'     redcap_info = redcap_info, fields_names = fields_names,
#'     fields_labels = fields_labels
#' )
#'

get_sheet_labels <- function(
    data, meta_data, redcap_info = c("record_id", "center"),
    fields_names = "field_name", fields_labels = "field_label"
) {

    assertive::assert_is_data.frame(data)
    assertive::assert_is_data.frame(meta_data)
    assertive::assert_is_character(redcap_info)
    assertive::assert_is_character(fields_names)
    assertive::assert_is_character(fields_labels)

    # Get patient and center id
    id <- redcap_info[1]
    center <- redcap_info[2]

    fn <- data %>%
        dplyr::select(
            - .data[[id]], - .data[[center]], - .data[["complete"]]
        ) %>%
        names()

    fl <- meta_data %>%
        dplyr::filter(.data[[fields_names]] %in% fn)

    fl[[fields_labels]] %>%
        stringr::str_to_title() %>%
        purrr::set_names(nm = fl[[fields_names]])
}
