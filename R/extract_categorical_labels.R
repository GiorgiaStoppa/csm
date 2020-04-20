#' Return the labels of categorical variables
#'
#' The functions returns the labels associated to each level of the
#' categorical variables.
#'
#' @param meta_data (tibble) the output from [tidy_extract] when
#'                          `type = 'meta'`
#' @param fields_names (character) a character with the name of the
#'                                 column of `meta_data` that contains
#'                                 the names of fields.
#' @param labels (character) a character with the name of the column of
#'                           `meta_data` that contains the labels
#'                           associated to the variables.
#'
#' @return (named list) a named list. Each element corresponds to the
#'                      labels associated to the categorical variables.
#'                      The names of the elements are the names of the
#'                      variables
#'
#' @export
#'
#' @examples
#' md <- tibble::tibble(
#'     field_name = c("age", "sex", "hypertension", "diabetes"),
#'     sheet = c("demo", "demo", "clinical", "clinical"),
#'     fct_level = list(
#'         character(0), c("female", "male"), c("yes", "no"),
#'         c("yes", "no")
#'     )
#' )
#'
#' extract_categorical_labels(md, "field_name", "fct_level")
#'

extract_categorical_labels <- function(
    meta_data, fields_names, labels
) {

    assertive::assert_is_data.frame(meta_data)
    assertive::assert_is_character(fields_names)
    assertive::assert_is_character(labels)

    nm <- names(meta_data)

    if (!(fields_names %in% nm | labels %in% nm)) {
        usethis::ui_stop(
            "'fields_names' or 'labels' are not included in the names of the meta-data. Please make sure you provided the correct names of the columns."
        )
    }

    if (!is.list(meta_data[[labels]])) {
        usethis::ui_stop(
            "The column with the labels of the fields must be a list. Please make sure that you provided the correct name of the column with the labels of the fields."
        )
    }

    dd <- meta_data %>%
        dplyr::select(.data[[fields_names]], .data[[labels]]) %>%
        dplyr::mutate(
            ll = purrr::map_dbl(.x = .data[[labels]], ~ length(.))
        ) %>%
        dplyr::filter(.data[["ll"]] != 0) %>%
        dplyr::select(-.data[["ll"]])

    dd[[labels]] %>%
        purrr::set_names(nm = dd[[fields_names]])

}

