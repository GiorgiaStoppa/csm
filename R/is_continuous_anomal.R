#' Check continuous out-of-range values
#'
#' Check if a continuous variable takes values that are outside the
#' range defined by two numerical values. The function returns logical
#' denoting which element of the vector is out of range.
#'
#' @param x (numeric) a numeric vector.
#'
#' @param range (numeric) a numeric vector with 2 values, i.e. the
#'                        minimum and the maximum allowed values for
#'                        `x`.
#'
#' @return (logical) a logical vector that identifies which element is
#'                   out of range.
#'
#' @export
#'
#' @examples
#'
#' x <- rnorm(n = 10L, mean = 10, sd = 2)
#' range   <- c(3, 12)
#' is_continuous_anomal(x = x, range = range)
#'
is_continuous_anomal <- function(x, range) {

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

    x < min_val | x > max_val
}

