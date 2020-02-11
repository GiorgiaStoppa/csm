#' Import the data from REDCap
#'
#' Import the data from REDCap repository into a data.frame format.
#' API token and url are needed to retrieve the data.
#'
#' @param token (character) a character string the specifies the token.
#'
#' @param url (character) a character string the specifies the url.
#'
#' @return (list) a list with 2 data.frame. One data.frame contains the
#'                data of the study, whereas the other contains three
#'                columns with the name of each field, i.e. the names
#'                of the variables in the first data.frame, the names of
#'                the form associated to each field and the labels
#'                associated to each field
#'
#' @examples
#'
#' url     <- "https://bbmc.ouhsc.edu/redcap/api/"
#' token   <- "9A81268476645C4E5F03428B8AC3AA7B"
#' csm:::read_redcap(url = url, token = token)
#'
read_redcap <- function(url = NULL, token = NULL) {

    assertive::is_not_null(token)
    assertive::is_not_null(url)
    assertive::assert_is_a_string(token)
    assertive::assert_is_a_string(url)

    # Import all the data from the study
    df <- REDCapR::redcap_read(
        redcap_uri = url, token = token,
        export_data_access_groups = TRUE,
        raw_or_label = "raw",
        raw_or_label_headers = "raw",
        export_checkbox_label = TRUE,
        guess_type = FALSE,
        verbose = FALSE
    )$data

    # Store the metadata into a separate object
    meta_data <- REDCapR::redcap_metadata_read(
        redcap_uri = url, token = token,
        verbose = FALSE
    )[["data"]] %>%
        dplyr::select(
            field_name, form_name, field_label,
            select_choices_or_calculations, field_note,
            text_validation_type_or_show_slider_number,
            text_validation_min, text_validation_max,
            branching_logic
        ) %>%
        dplyr::rename(
            field_type = text_validation_type_or_show_slider_number,
            validation_min = text_validation_min,
            validation_max = text_validation_max
        )

    list(
        "study_data" = df,
        "study_metadata" = meta_data
    )

}
