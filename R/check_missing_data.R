#' Returns the fields with missing information for each patient
#'
#' The function returns the fields that are missing for each patient
#' in a given form.
#'
#' @param nested_tables (tibble) the output from [nest_tables]
#' @param meta_data (tibble) the output from [tidy_extract] when
#'                          `type = 'meta'`
#' @param tab (character) the name of the sheet on which missing data
#'                        check must be performed.
#' @param fields_names (character) a character with the name of the
#'                                column of `meta_data` that contains
#'                                the names of fields.
#' @param branching_logic (character)) a character with the name of the
#'                                     column of `meta_data` that the
#'                                     branching logic associated to
#'                                     each field.
#' @param redcap_info (character) the names of the variables that
#'                                identify the subject and the site
#'                                where the subject was enrolled.
#'
#' @return [tibble][tibble::tibble-package] a tibble that identifies the
#'                                          missing fields in a given
#'                                          form for each patient
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   nested <- readr::read_rds(db_update_from_server())
#'
#'   get_incomplete_sheets(nested, redcap_info = c("id", "center"))
#' }
#'

check_missing <- function(
    nested_tables, meta_data, tab, fields_names,
    branching_logic, redcap_info = c("record_id", "center")
) {

    assertive::assert_is_data.frame(nested_tables)
    assertive::assert_is_data.frame(meta_data)
    assertive::assert_is_character(tab)
    assertive::assert_is_character(fields_names)
    assertive::assert_is_character(branching_logic)

    # Retrieve names of center and id columns
    id <- redcap_info[1]
    center <- redcap_info[2]

    # Select the sheet on which missing data checking is performed
    dd <- get_sheet(nested_tables, sheet = tab)

    # Select fields on which missing data checking must be performed
    cols_br <- meta_data %>%
        dplyr::filter(.data[["sheet"]] == tab) %>%
        dplyr::select(
            .data[[fields_names]], .data[[branching_logic]]
        ) %>%
        dplyr::filter(is.na(.data[[branching_logic]]))

    cols_br <- cols_br[[fields_names]]

    dd %>%
        dplyr::select(
            .data[[center]],
            .data[[id]],
            dplyr::contains(cols_br)
        ) %>%
        dplyr::group_by(.data[[center]], .data[[id]]) %>%
        tidyr::nest(missing_vars = -c(!! enquo(center), !! enquo(id))) %>%
        dplyr::mutate(
            missing_vars = purrr::map(
                .x = .data$missing_vars,
                ~ get_missing_vars(data = .x)
            )
        ) %>%
        tidyr::unnest(cols = .data$missing_vars)
}

