#' Returns the fields with anomal values in continuous variables
#' for each patient
#'
#' The function returns the fields of continuous variables with anomal
#' values for each patient in a given form.
#'
#' @param nested_tables (tibble) the output from [nest_tables]
#' @param meta_data (tibble) the output from [tidy_extract] when
#'                          `type = 'meta'`
#' @param redcap_info (character) the names of the variables that
#'                                identify the subject and the site
#'                                where the subject was enrolled.
#' @param tab (character) the name of the sheet on which missing data
#'                        check must be performed.
#' @param fields_names (character) a character with the name of the
#'                                column of `meta_data` that contains
#'                                the names of fields.
#' @param range_low (character) a character with the name of the column of
#'                              `meta_data` that contains the lower
#'                              bound of the range associated to each
#'                              continuous variable.
#' @param range_upp (character) a character with the name of the column of
#'                              `meta_data` that contains the upper
#'                              bound of the range associated to each
#'                              continuous variable.
#'
#' @return [tibble][tibble::tibble-package] a tibble that identifies the
#'                                          fields of continuous
#'                                          variables with anomal
#'                                          values
#'
#' @export
#'
#' @examples
#' \dontrun{
#'    check_continuous()
#' }
#'

check_continuous <- function(
    nested_tables, meta_data, redcap_info = c("record_id", "center"),
    tab, fields_names, range_low, range_upp
) {

    assertive::assert_is_data.frame(nested_tables)
    assertive::assert_is_data.frame(meta_data)
    assertive::assert_is_character(tab)
    assertive::assert_is_character(fields_names)
    assertive::assert_is_character(range_low)
    assertive::assert_is_character(range_upp)
    assertive::assert_is_character(redcap_info)

    # Retrieve names of center and id columns
    id <- redcap_info[1]
    center <- redcap_info[2]

    # Select the sheet on anomal values checking is performed
    dd <- get_sheet(nested_tables, sheet = tab)

    # Returns only the meta-data to the associated sheet
    md_dd <- meta_data %>%
        dplyr::filter(.data[["sheet"]] == tab)

    # Retrieve continuous range
    range <- extract_range(md_dd, fields_names, range_low, range_upp)

    if (length(range) == 0) {

        tibble::tibble(
            center = character(),
            pat_id = character(),
            variables = character()
        ) %>%
            dplyr::rename_at(
                dplyr::vars(.data[["pat_id"]]),
                ~ stringr::str_replace(string = ., "pat_id", id)
            )

    } else {

        # Check categorical
        dd %>%
            dplyr::select(
                dplyr::contains(center),
                dplyr::contains(id),
                dplyr::contains(names(range))
            ) %>%
            dplyr::group_by(.data[[id]], .data[[center]]) %>%
            tidyr::nest(variables = dplyr::contains(names(range))) %>%
            dplyr::mutate(
                variables = purrr::map(
                    .x = .data[["variables"]],
                    ~ which_out_range(data = .x, range = range)
                )
            ) %>%
            tidyr::unnest(cols = .data[["variables"]]) %>%
            dplyr::ungroup()
    }
}

