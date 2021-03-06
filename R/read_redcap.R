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
read_redcap <- function(url, token) {

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
        dplyr::rename(
            validation_min = .data$text_validation_min,
            validation_max = .data$text_validation_max
        )

    list(
        "data" = df,
        "meta_data" = meta_data
    )

}
