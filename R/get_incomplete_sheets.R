#' Returns the incomplete sheets for each patients
#'
#' The function returns the sheets that are marked as incomplete or
#' unverified for each patients enrolled in the study.
#'
#' @param nested_tables (tibble) the output from [nest_tables]
#' @param redcap_info (character) the names of the variables that
#'                                identify the subject and the site
#'                                where the subject was enrolled.
#'
#' @return [tibble][tibble::tibble-package] a tibble that identifies the
#'                                          incomplete or unverified
#'                                          sheets for all the patients
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

get_incomplete_sheets <- function(
    nested_tables, redcap_info = c("record_id", "center")
) {

    assertive::assert_is_data.frame(nested_tables)
    assertive::assert_is_character(redcap_info)

    # Redcap info into separate objects
    id <- redcap_info[1]
    center <- redcap_info[2]

    # Get the names of the sheets
    sheet_names <- nested_tables[["sheets"]]
    field_names <- nested_tables[["fields"]]

    # Iterate the function incomplete_sheet for all the sheets
    purrr::map2_dfr(
        .x = sheet_names, .y = field_names,
        ~ incomplete_sheet(
            nested_tables, tab = .x, field = .y, redcap_info
        )
    )
}

