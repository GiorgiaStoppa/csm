#' Get the branching logic conditions
#'
#' Get the conditions for missing data checking. If a variable has no
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
#'
#' @examples
#' \dontrun{
#'
#'  meta_data <- tibble::tibble(
#'    field_name = c("id", "sex", "diabetes", "iddm"),
#'    sheet = c(rep("demo", 2L), rep("clinical", 2L)),
#'    branching_logic = c(rep(NA_character_, 3L), "diabetes")
#'  )
#'
#'  get_branching_logic(
#'    meta_data = meta_data,
#'    fields_names = "field_name",
#'    branching_logic = "branching_logic"
#'  )
#'
#' }

get_branching_logic <- function(
    meta_data, fields_names, branching_logic
) {

    assertive::assert_is_data.frame(meta_data)
    assertive::assert_is_character(fields_names)
    assertive::assert_is_character(branching_logic)

    f_nms <- paste(
        "(",
        paste0(meta_data[[fields_names]], collapse = ")|("),
        ")",
        sep = ""
    )

    fn <- dplyr::enquo(fields_names)
    br <- dplyr::enquo(branching_logic)

    br_df <- meta_data %>%
        dplyr::select(!! fn, !! br) %>%
        dplyr::mutate(
           !! br := stringr::str_extract(
               .data[[branching_logic]], f_nms
           )
        ) %>%
        tidyr::drop_na(!! br)

    purrr::set_names(
        x = br_df[[fields_names]],
        nm = br_df[[branching_logic]]
    )

}

