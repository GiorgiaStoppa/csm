#' Returns the fields with missing information for a patient in a form
#'
#' The function returns the fields that are missing for a patient
#' in a given form.
#'
#' @param data (tibble) a tibble with one row and as many columns as the
#'                      fields in a given form
#'
#' @return a (character) with the names of the missing fields for the
#'         patient.
#'
#' @export
#'
#' @examples
#'
#' df <- tibble::tibble(
#'   age = 63,
#'   sex = "female",
#'   diabetes = NA_character_,
#'   hypertension = "yes",
#'   nyha = NA_character_
#' )
#'
#' get_missing_vars(df) # c("diabetes", "nyha")
#'

get_missing_vars <- function(data) {

    assertive::assert_is_data.frame(data)

    miss_df <- purrr::imap_dfr(
            .x = data,
            ~ tibble::tibble(
                variable = .y, is_missing = is.na(.x)
            )
        ) %>%
            dplyr::filter(.data$is_missing)

    miss_df[["variable"]]
}
