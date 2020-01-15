#' Import the data from REDCap
#'
#' Import the data from REDCap repository into a data.frame format.
#' API token and url are needed to retrieve the data.
#'
#' @param token (character) a character string the specifies the token.
#'
#' @param url (character) a character string the specifies the url.
#'
#' @return (data.frame) The data of the study stored as a data.frame.
#'
#' @examples
#'
#' url     <- "https://bbmc.ouhsc.edu/redcap/api/"
#' token   <- "9A81268476645C4E5F03428B8AC3AA7B"
#' csm:::import_redcap(token = token, url = url)
#'
import_redcap <- function(token, url) {

    assertive::is_a_string(token)
    assertive::is_a_string(url)

    # Import all the field of ROLEX db
    REDCapR::redcap_read(
        redcap_uri = url, token = token,
        export_data_access_groups = TRUE,
        raw_or_label = "raw",
        raw_or_label_headers = "raw",
        export_checkbox_label = TRUE,
        guess_type = FALSE,
        verbose = FALSE
    )$data

}
