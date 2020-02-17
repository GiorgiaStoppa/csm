#' Check continuous out-of-range values
#'
#' Check if a continuous variable takes values that are outside the
#' range defined by two numerical values. The function returns a
#' numeric vector with the values of the variable that lie outside of
#' the range.
#'
#' @param x (numeric) a numeric vector.
#'
#' @param range (numeric) a numeric vector with 2 values, i.e. the
#'                        minimum and the maximum allowed values for
#'                        `x`.
#'
#' @return (numeric) a numeric vector with all the elements of `x` that
#'                   are not within the `range`. If no value is
#'                   identified a `NA` is returned.
#'
#' @examples
#'
#' x <- rnorm(n = 10L, mean = 10, sd = 2)
#' range   <- c(3, 12)
#' csm:::continuous_anomal_values(x = x, range = range)
#'
continuous_anomal_values <- function(x, range) {

    assertive::assert_is_numeric(x)
    assertive::assert_is_numeric(range)

    min_val <- range[[1]]
    max_val <- range[[2]]

    if (max_val < min_val) {
        warning("Max in range is below the min. They will be switched.")
        aux <- max_val
        max_val <- min_val
        min_val <- aux
    }

    out_of_range <- x < min_val | x > max_val

    x[out_of_range]

}

