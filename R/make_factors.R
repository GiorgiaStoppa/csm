#' Make factors
#'
#' Variables of a data frame are transformed into factors. For each
#' variable that must be transformed as factor, a vector of the
#' corresponding levels labels is provided.
#'
#' @param x (data frame)
#' @param lev (named list) of levels. All the names must be included in
#'   variables' names of `x`
#'
#' @return a new version of x, of the same class, with the required
#'   variables mutated to factors
#' @export
#'
#' @examples
#' library(tibble)
#'
#' df <- tibble(
#'   foo = c("a", "a", "b"),
#'   bar = 1:3,
#'   baz = c("one", "two", "three"),
#'   qux = c("random", "sample", "text")
#' )
#'
#' fct_levels <- list(
#'   foo = c("a", "b", "c", "d"),
#'   baz = c("one", "two", "three")
#' )
#'
#' make_factors(df, fct_levels)
make_factors <- function(x, lev) {

    assertive::assert_is_data.frame(x)
    assertive::assert_is_list(lev)

    for (var in names(lev)) {
        x[[var]] <- factor(x[[var]], lev[[var]])
    }
    x
}
