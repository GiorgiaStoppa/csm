#' Exclude absent elements
#'
#' Given a target vector with elements and a reference vector with
#' named elements, it returns all the elements from the target
#' vector that are not present in the reference vector or that, if
#' they appear in the reference vector, the associated names are
#' themselves included in the target vector.
#'
#' @details the function (after it checks the elements from the
#' reference vector are not included in the target vector) implements
#' the subsetting of a character vector throughout the names of its
#' elements (https://adv-r.hadley.nz/subsetting.html#subset-multiple).
#' Moreover, [`%in%`] takes as [FALSE] the presence of a [NA]
#' (see *Details* section of [`%in%`])
#'
#' @param target (chr) vector with elements whose inclusion must be
#'                     checked
#' @param reference (named chr) vector with reference names
#'
#' @return (chr) vector with the remaining elements (subset of the
#'               target) after the checking has been performed
#'
#' @export
#'
#' @examples
#' matteo <- c("c", "d", "f", "g", "h")
#' paolo <- c("a", "b", "c", "d", "f", "g", "h")
#' stefano <- c("a", "c", "d", "f", "g", "h")
#' davide <- purrr::set_names(x = c("f", "g"), nm = c("a", "b"))
#'
#' exclude_absent(matteo, davide) # c("c", "d", "h")
#' exclude_absent(paolo, davide)  # paolo
#' exclude_absent(stefano, davide) # c("a", "c", "d", "f", "h")
#'
exclude_absent <- function(target, reference) {

  assertive::assert_is_character(target)
  assertive::assert_is_character(reference)

  if(is.null(names(reference))) {
    usethis::ui_stop("'reference' must be a named vector")
  }

  target[(!target %in% reference) | (target %in% reference[target])]
}

