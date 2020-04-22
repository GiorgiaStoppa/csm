#' Returns the fields with missing information for a grouping
#' variable(s) in a form
#'
#' The function returns the fields that are missing in a given form for
#' one or more grouping variables.
#'
#' @param data (tibble) a tibble with one row and as many columns as the
#'                      fields in a given form
#'
#' @param group (character) the name(s) of the variable(s) by which the
#'                          data.frame should be grouped
#'
#' @return [tibble][tibble::tibble-package] a tibble with one column
#'                                          with the values of the
#'                                          grouping variable and one
#'                                          column with the missing
#'                                          fields associated to each
#'                                          value of the grouping
#'                                          variable
#'
#' @export
#'
#' @examples
#'
#' data <- tibble::tibble(
#'     id = c("id_1", "id_2", "id_3"),
#'     age = c(63, 65, 66),
#'     sex = c("female", NA_character_, "male"),
#'     diabetes = c(NA_character_, "yes", "no"),
#'     hypertension = c("yes", NA_character_, NA_character_),
#'     nyha = rep(NA_character_, 3L)
#' )
#'
#' get_missing_vars_by(data, "id")
#'

get_missing_vars_by <- function(data, group) {

    assertive::assert_is_data.frame(data)
    assertive::assert_is_character(group)

    if(!group %in% names(data)) {
        usethis::ui_stop(
            "The grouping variable must be included in 'data'"
        )
    }

    data %>%
        dplyr::group_by(.data[[group]]) %>%
        dplyr::mutate_at(
            dplyr::vars(-.data[[group]]),
            ~ is.na(.)
        ) %>%
        tidyr::pivot_longer(
            cols = -.data[[group]],
            names_to = "missing_vars",
            values_to = "is_missing"
        ) %>%
        dplyr::filter(.data[["is_missing"]]) %>%
        dplyr::select(-.data[["is_missing"]]) %>%
        dplyr::ungroup()

}
