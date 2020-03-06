#' Create the branching logic conditions
#'
#' Create the conditions for missing data checking. If a variable has no
#' branching logic and it is NA, then the information is missing. If a
#' variable is missing and it has a branching logic, then it is missing
#' only if it is missing and the branching logic is not satisfied.
#'
#' @param meta_data (tibble) a tibble containing the meta_data
#'                           information of the study.
#'
#' @param fields_names (character) a character with the name of the
#'                                column of `meta_data` that contains
#'                                the names of fields.
#'
#' @param branching_logic (character)) a character with the name of the
#'                                     column of `meta_data` that the
#'                                     branching logics associated to
#'                                     each field.
#'
#' @return (tibble) a tibble with two columns: one column must contains
#'                  the names of the fields. The other column must
#'                  contains the associated condition for missing data
#'                  checking; if it is NA, then it means that no
#'                  branching logic is associated to the field.
#'
#' @examples

branching_logic <- function(
    meta_data, fields_names, branching_logic
) {

    assertive::assert_is_data.frame(meta_data)
    assertive::assert_is_character(fields_names)
    assertive::assert_is_character(branching_logic)

    dd <- meta_data[, c(fields_names, branching_logic)] %>%
        dplyr::mutate(
            # Create the variable condition

        )



}

