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
#'                  checking.
#' @export
#'
#' @examples
#' \dontrun{
#'
#'  library(tibble)
#'
#'  meta_data <- tibble(
#'    field_name = c("id", "sex", "diabetes", "iddm"),
#'    sheet = c(rep("demo", 2L), rep("clinical", 2L)),
#'    branching_logic = c(rep(NA_character_, 3L), "diabetes")
#'  )
#'
#'  branching_logic(
#'    meta_data = meta_data,
#'    fields_names = "field_name",
#'    branching_logic = "branching_logic"
#'  )
#'
#' }

branching_logic <- function(
    meta_data, fields_names, branching_logic
) {

    assertive::assert_is_data.frame(meta_data)
    assertive::assert_is_character(fields_names)
    assertive::assert_is_character(branching_logic)

    f_nms <- meta_data[[fields_names]] %>%
        paste0(collapse = ")|(") %>%
        paste("(", ., ")", sep = "")

    fields_names <- dplyr::enquo(fields_names)
    branching_logic <- dplyr::enquo(branching_logic)

    meta_data %>%
        dplyr::select(!! fields_names, !! branching_logic) %>%
        dplyr::mutate(
           !! branching_logic := str_extract(
               .data$branching_logic, f_nms
           )
        ) %>%
        tidyr::drop_na(!! branching_logic)
}

