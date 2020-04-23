#' Return the patients with the incomplete (or unverified) sheet
#'
#' The function returns the patients whose form was marked as
#' incomplete or unverified.
#'
#' @param nested_tables (tibble) the output from [nest_tables]
#' @param tab (chr) the name of the sheet targeted by the check
#' @param field (chr) the name of the field which the targeted sheet
#'                    belongs to.
#' @param redcap_info (character) the names of the variables that
#'                                identify the subject and the site
#'                                where the subject was enrolled.
#'
#' @return [tibble][tibble::tibble-package] a tibble that identifies the
#'                                          patients with incomplete
#'                                          or unverified sheet
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   nested <- readr::read_rds(db_update_from_server())
#'
#'   incomplete_sheet(
#'     nested, "demographics", "demographics and clinical",
#'     redcap_info = c("id", "center")
#'   )
#' }
#'

incomplete_sheet <- function(
    nested_tables, tab, field, redcap_info = c("record_id", "center")
) {

    assertive::assert_is_data.frame(nested_tables)
    assertive::assert_is_character(tab)
    assertive::assert_is_character(redcap_info)

    # Redcap info into separate objects
    id <- redcap_info[1]
    center <- redcap_info[2]

    # Retrieve the patients with incomplete form
    get_sheet(nested_tables, sheet = tab, field = field) %>%
        dplyr::select(
            .data[[center]], .data[[id]], .data[["complete"]]
        ) %>%
        dplyr::filter(
            .data[["complete"]] %in% c("incomplete", "unverified")
        ) %>%
        dplyr::mutate(sheet = tab)

}

