#' Check which continuous variable has an out-of-range value
#'
#' For each patient enrolled in the study, the function checks which
#' continuous variable assumes a value that it is not included in the
#' range.
#'
#' @param data (data.frame) a dataframe with the continuous variables
#'                          for each patient.
#'
#' @param range (character) output of the function `extract_range`.
#'
#' @return (character) a character vector with the names of the
#'                     continuous variables that had an out-of-range
#'                     value.
#'
#' @export
#'
#' @examples
#'
#' range <- list("age" = c(18, 70), "hemoglobin" = c(6, 18))
#' df <- tibble::tibble(age = 16, hemoglobin = 9)
#'
#' which_out_range(data = df, range = range)
#'

which_out_range <- function(data, range) {

    assertive::assert_is_list(range)
    assertive::assert_is_data.frame(data)

    if(length(setdiff(names(data), names(range)))) {
        usethis::ui_stop(
            "Names of the data and names of the range are different. Please make sure that you correctly specify the columns to which checking must be performed."
        )
    }

    vars_df <- purrr::map2_df(
        .x = data, .y = range,
        ~ is_continuous_anomal(.x, .y)
    ) %>%
        tidyr::pivot_longer(
            cols = dplyr::everything(),
            names_to = "variable",
            values_to = "is_anomal"
        ) %>%
        dplyr::filter(.data[["is_anomal"]])

    vars_df[["variable"]]

}
