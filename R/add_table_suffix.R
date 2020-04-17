#' Add suffix to columns names
#'
#' Given a data.frame and a string, the functions add to each column's
#' name of the data the string as suffix.
#'
#' @param data (data.frame) a data.frame
#'
#' @param suffix (character) a character vector with a single element
#'
#' @param except (character) a character vector whose elements represent
#'                           the columns names of (data) the suffix will
#'                           not be added to.
#'
#' @return [tibble][tibble::tibble-package] a tibble whose columns names
#'                                          have a suffix equal to
#'                                          (suffix) but those included
#'                                          in (except).
#'
#' @details The functions has been implemented to as helper to create
#'          a unique data.frame with all the forms of a single study.
#'          The functions ensures that all the fields will have a
#'          unique and it avoids to have duplicated names, which can
#'          likely occur for example if more follow-up dates of events
#'          are recorded.
#'
#' @export
#'
#' @examples
#'
#' set.seed(1)
#'
#' df <- tibble::tibble(
#'     center = rep(c("center_1", "center_2"), each = 3L),
#'     id = c(
#'         "center_1_id_1", "center_1_id_2", "center_1_id_3",
#'         "center_2_id_1", "center_2_id_2", "center_2_id_3"
#'     ),
#'     age = rnorm(6, 60, 15),
#'     sex = sample(c("female", "male"), 6, TRUE)
#' )
#'
#' suffix <- "demo"
#' except <- c("id", "center")
#'
#' add_table_suffix(df, suffix, except)
#'

add_table_suffix <- function(data, suffix, except) {

    assertive::assert_is_data.frame(data)
    assertive::assert_is_character(suffix)
    assertive::assert_is_character(except)

    if (!all(except %in% names(data))) {
        usethis::ui_stop(
            "'except' must be included in the columns names."
        )
    }

    if (length(suffix) != 1) {
        usethis::ui_stop(
            "'except' must be a single element character."
        )
    }

    data %>%
        dplyr::rename_at(
            dplyr::vars(-dplyr::contains(except)),
            ~ glue::glue("{.}_{suffix}")
        )

}

