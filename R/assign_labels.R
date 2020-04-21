#' Assign the labels to the elements of a character
#'
#' Given a character vector `x` and a named character vector `y` whose
#' names correspond to the elements contained in `x`, the function
#' switches the elements of `x` with the elements of `y`.
#'
#' @param x (character) a character vector.
#' @param y (character) a named character vector whose names must be
#'                      be contained in `x`.
#' @return a (character) vector.
#'
#' @details The function is intended to be used when the report of
#'          data quality check is implemented. When some fields are
#'          shown in the tables, it could be useful to report their
#'          labels for ease of readibility.
#'
#' @export
#'
#' @examples
#'
#' x <- c("a", "b", "c")
#' y <- c("1", "2", "3") %>%
#'     purrr::set_names(nm = c("a", "b", "c"))
#'
#' assign_labels(x, y)
#'

assign_labels <- function(x, y) {


    assertive::assert_is_character(x)
    assertive::assert_is_character(y)

    if(!all(x %in% names(y))) {
        usethis::ui_stop(
            "All the elements of 'x' must be included in the names of 'y'"
        )
    }

    unname(y[match(x, names(y))])

}
