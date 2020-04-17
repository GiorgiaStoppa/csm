#' Tidy the names of the fields
#'
#' The function takes a character vector which elements are the names
#' of the fields for which the information is missing. If a field has
#' more than one column, it remove the duplicated columns.
#'
#' @param data (tibble) a tibble with one column that contains the
#'                      names of the fields with missing information
#'
#' @return [tibble][tibble::tibble-package] a tibble with one column
#'                                          that contains the names with
#'                                          missing information without
#'                                          duplicates
#'
#' @examples
#'
#' \dontrun{
#'
#' a <- tibble::tibble(
#'   variable = c("aa", "dd_1", "dd_2", "dd_3", "cc")
#' )
#'
#' tidy_fields_names(data = a)
#'
#' }
#'

tidy_fields_names <- function(data) {

    assertive::assert_is_data.frame(data)

    if(ncol(data) > 1) {
        usethis::ui_stop("The tibble must has only one column")
    }

    if(names(data) != "variable") {
        usethis::ui_stop("The name of the column must be 'variable'")
    }

    if(!is.character(data[[1]])) {
        usethis::ui_stop("The column of the tibble must be a character")
    }

    data %>%
        dplyr::mutate(variable = remove_last_number(.data$variable)) %>%
        dplyr::group_by(.data$variable) %>%
        dplyr::summarise(n = dplyr::n())%>%
        dplyr::ungroup() %>%
        dplyr::select(.data$variable)
}

