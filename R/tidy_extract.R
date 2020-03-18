#' Extract and tidy data from REDCap import.
#'
#' The function extract the data or the metadata from the output
#' returned by [read_redcap]. After the extraction, variables names
#' are cleaned, textual columns are lowered, and the event fields
#' like "fields" are renamed.
#'
#' @param data the output of [read_redcap]
#' @param type (chr) it can takes two values: "data" or "meta". If
#'                   "data" is passed, then the data of the study are
#'                   returned. If "meta" is passed, then the meta-data
#'                   of the study are returned.
#'
#' @return [tibble][tibble::tibble-package]
#' @export
#'
#' @examples
#' \dontrun{
#'
#' url     <- "https://bbmc.ouhsc.edu/redcap/api/"
#' token   <- "9A81268476645C4E5F03428B8AC3AA7B"
#' data <- read_redcap(url = url, token = token)
#'
#' tidy_extract(data, type = "data")
#' tidy_extract(data, type = "meta")
#'
#' }
#'
tidy_extract <- function(data, type = c("data", "meta")) {

    assertive::assert_is_list(data)
    assertive::assert_is_list(data[["data"]])
    assertive::assert_is_list(data[["meta_data"]])
    assertive::assert_is_data.frame(data[["data"]][["data"]])
    assertive::assert_is_data.frame(data[["meta_data"]][["data"]])
    assertive::assert_is_character(type)

    type <- match.arg(type)

    if (type == "meta") {
        type <- "meta_data"
    }

    res <- data[[type]][["data"]] %>%
        tibble::as_tibble() %>%
        janitor::clean_names() %>%
        dplyr::mutate_if(is.character, tolower)

    if (type == "data") {
        res <- res %>%
            dplyr::rename(
                center = .data$redcap_data_access_group,
                fields = .data$redcap_event_name
            )
    }

    if (type == "meta_data") {
        res <- res %>%
            dplyr::rename(
                sheet = .data$form_name,
                fct_level = .data$select_choices_or_calculations
            ) %>%
            dplyr::mutate(
                fct_level = purrr::map2(.data$fct_level, .data$field_type, ~{
                    dplyr::case_when(
                        !is.na(.x) & !(.y %in% c("calc", "text")) ~ str_to_level(.x),
                        TRUE ~ .x
                    )
                }) %>%
                    purrr::set_names(.data$field_name),

                sheet = stringr::str_replace(
                    .data$sheet,
                    "_da_compilare.*$",
                    ""
                )
            )
    }

    janitor::remove_empty(res, c("rows", "cols"))
}
