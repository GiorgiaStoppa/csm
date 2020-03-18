#' Check categorical impossible values
#'
#' Check if a categorical variable takes values that are different from
#' these allowed by the form of Redcap. The function returns a
#' logical that identifies which elements of the variable assume an
#' impossible.
#'
#' @param x (character) a character vector.
#'
#' @param values (character) a character vector with the allowed values
#'                           for `x`.
#'
#' @return (character) a character vector with all the elements of `x`
#'                     that are different from `values`. If no values a
#'                     `NA_character_` is returned.
#'
#' @export
#'
#' @examples
#'
#' gender <- c("male", "female", "male", "male", "female", "x")
#' values <- c("male", "female")
#' csm:::categorical_anomal_values(x = gender, values = values)
#'
categorical_anomal_values <- function(x, values) {

    assertive::assert_is_character(x)
    assertive::assert_is_character(values)

    wrong_values <- !x %in% values

    x[wrong_values]

}

