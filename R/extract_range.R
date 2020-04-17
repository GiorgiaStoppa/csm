#' Extract the range of continuous variables
#'
#' The functions returns range associated to each continuous variable.
#'
#' @param meta_data (tibble) the output from [tidy_extract] when
#'                          `type = 'meta'`
#' @param fields_names (character) a character with the name of the
#'                                 column of `meta_data` that contains
#'                                 the names of fields.
#' @param range_low (character) a character with the name of the column of
#'                              `meta_data` that contains the lower
#'                              bound of the range associated to each
#'                              continuous variable.
#'
#' @param range_upp (character) a character with the name of the column of
#'                              `meta_data` that contains the upper
#'                              bound of the range associated to each
#'                              continuous variable.
#'
#' @return (named list) a named list. Each element corresponds to the
#'                      range associated to the continuous variables.
#'                      The names of the elements are the names of the
#'                      variables
#'
#' @export
#'
#' @examples
#' md <- tibble::tibble(
#'     field_name = c("age", "sex", "hypertension", "diabetes"),
#'     sheet = c("demo", "demo", "clinical", "clinica"),
#'     min = c(18, NA, NA, NA),
#'     max = c(75, NA, NA, NA)
#' )
#'
#' extract_range(md, "field_name", "min", "max")
#'

extract_range <- function(
    meta_data, fields_names,
    range_low = "text_validation_min",
    range_upp = "text_validation_max"
) {

    assertive::assert_is_data.frame(meta_data)
    assertive::assert_is_character(fields_names)
    assertive::assert_is_character(range_low)
    assertive::assert_is_character(range_upp)

    nm <- names(meta_data)

    if (
        !fields_names %in% nm | !range_low %in% nm | !range_upp %in% nm
    ) {
        usethis::ui_stop(
            "'fields_names' or 'range_low' or 'range_upp' are not included in the names of the meta-data. Please make sure you provided the correct names of the columns."
        )
    }

    dd <- meta_data %>%
        dplyr::select(
            .data[[fields_names]], .data[[range_low]],
            .data[[range_upp]]
        ) %>%
        dplyr::filter(
            !(is.na(.data[[range_low]]) & is.na(.data[[range_upp]]))
        ) %>%
        dplyr::mutate(
            lower = as.double(.data[[range_low]]),
            upper = as.double(.data[[range_upp]])
        ) %>%
        dplyr::mutate(
            range = purrr::map2(
                .x = .data[["lower"]], .y = .data[["upper"]],
                ~ c(.x, .y)
            )
        )

    dd[["range"]] %>%
        purrr::set_names(nm = dd[[fields_names]])

}

