#' Remove last numbers
#'
#' The function takes a character vector and it removes the last two
#' positions of the element, if the an element ends with a "_" and a
#' number.
#'
#' @param x (chr) a character vector
#'
#' @return (chr) a character vector
#'
#' @examples
#'
#' \dontrun{
#'
#' a <- c("aa", "bb", "cc", "dd_1", "dd_2", "ff")
#'
#' remove_last_number(x = a)
#'
#' }
#'

remove_last_number <- function(x) {

    assertive::assert_is_character(x)

    stringr::str_remove(x, pattern = "_[0-9]+$")
}

