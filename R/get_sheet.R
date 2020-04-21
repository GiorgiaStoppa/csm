#' Get a sheet table
#'
#' The function retrieves a data.frame with the data of a sheet
#' associated to a given field.
#'
#' @param x (tbl_df) output from [db_update_from_server] or
#'                   [nest_tables].
#' @param sheet (chr) name of sheet to get the data from
#' @param field (chr, default = NULL) if not NULL, provide the name of
#'   field containing the required sheet. If NULL, only one field
#'   containing the required sheet is supposed to be present; that
#'   field is used. If more than one field with the required sheet
#'   exists, and field is NULL, an error is thrown, suggesting the
#'   possible fields to consider.
#'
#' @return the required sheet [tibble][tibble::tibble-package]
#' @export
#'
#' @examples
#' \dontrun{
#'   library(readr)
#'
#'   data <- read_rds(db_update_from_server())
#'   data %>% get_sheet("demographic")
#'   data %>% get_sheet("demographic", field = "demo and clinical")
#'
#' }
get_sheet <- function(x, sheet, field = NULL) {

    assertive::assert_is_data.frame(x)
    assertive::assert_is_character(sheet)

    if(!is.null(field) & !is.character(field)) {
        usethis::ui_stop(
            "If the field is not NULL, it must be a character"
        )
    }

    where_sheet <- sheet == x[["sheets"]]
    n_sheets <- sum(where_sheet)

    if (n_sheets == 0) usethis::ui_stop(
        "Sheet {ui_value(sheet)} does not exists in data."
    )


    possible_fields <- x[["fields"]][where_sheet]

    if (!is.null(field) && (!field %in% possible_fields)) {
        usethis::ui_stop("
      Sheet {ui_value(sheet)} is not present in the field {ui_value(field)}.
      Try to select one field of {ui_value(possible_fields)}.
    ")
    }

    if (n_sheets > 1 && is.null(field)) {
        usethis::ui_stop("
      Sheet {ui_value(sheet)} is present in the {ui_field(possible_fields)}.
      Please, provide the {ui_field('field')} argument to {ui_code('get_table()')}.
    ")
    }

    sheet_row <- if (is.null(field)) {
        where_sheet
    } else {
        where_sheet & (field == x[["fields"]])
    }

    x[["tables"]][[which(sheet_row)]]
}
