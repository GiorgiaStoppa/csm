#' Extract tidy data of REDCap db
#'
#' Extract a the tibble with the data from [read_redcap] output and tidy
#' it.
#'
#' @param data (list) the output of [read_redcap].
#'
#' @return [tibble][tibble::tibble-package] a tibble that contains the
#'                                          tidied data of the study.
#' @export
#'
#' @examples
#' library(tibble)
#'
#' dd <- tibble(
#'     id = c(rep("1", 2L), rep("2", 2L), rep("3", 2L)),
#'     redcap_event_name = rep(c("demo and clinical", "discharge"), 3L),
#'     sex = c(
#'         "female", NA_character_,
#'         "male", NA_character_,
#'         "female", NA_character_
#'     ),
#'     age = c(
#'         44, NA_real_,
#'         50, NA_real_,
#'         71, NA_real_
#'     ),
#'     demo_complete = c(
#'         "complete", NA_character_,
#'         "complete", NA_character_,
#'         "complete", NA_character_
#'     ),
#'     hp = c(
#'         "yes", NA_character_,
#'         "no", NA_character_,
#'         "yes", NA_character_
#'     ),
#'     diabetes = c(
#'         "no", NA_character_,
#'         "no", NA_character_,
#'         "yes", NA_character_
#'     ),
#'     clinical_complete = c(
#'         "complete", NA_character_,
#'         "complete", NA_character_,
#'         "complete", NA_character_
#'     ),
#'     fe = c(
#'         NA_real_, 35,
#'         NA_real_, 56,
#'         NA_real_, 44
#'     ),
#'     death = c(
#'         NA_character_, "no",
#'         NA_character_, "no",
#'         NA_character_, "yes"
#'     ),
#'     discharge_complete = c(
#'         NA_character_, "complete",
#'         NA_character_, "complete",
#'         NA_character_, "complete"
#'     )
#' )
#' md <- tibble::tibble(
#'     field_name = c("sex", "age", "hp", "diabetes", "fe", "death"),
#'     form_name = c(rep("demo and clinical", 4L), rep("discharge", 2L))
#' )
#'
#' study_data <- list("data" = dd, "meta_data" = md)
#'
#' tidy_data_extraction(data = study_data)
tidy_data_extraction <- function(data) {

    assertive::assert_is_list(data)

    tibble::as_tibble(data[["data"]]) %>%
        janitor::clean_names() %>%
        dplyr::mutate_if(is.character, tolower) %>%
        dplyr::rename(fields = .data$redcap_event_name)

}


