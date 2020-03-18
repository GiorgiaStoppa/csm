#' Get the data of the study from the REDCap server
#'
#' @param token (chr) a character string that specifies the token to
#'                    get access to the data of the study
#'
#' @param path_data (chr) path for data folder (default is `data/`
#'   under the current project or the [here][here::here] path. If the
#'   folder does not exist, it will be created.)
#'
#' @param redcap_info (character) the names of the variables that
#'                                identify the subject and the site
#'                                where the subject was enrolled.
#'
#' @param file_name (chr) file name in which data are stored.
#'
#' @return (lgl) `TRUE` if success, `FALSE` otherwise
#' @export
#'
#' @examples
#' \dontrun{
#'   db_update_from_server()
#' }
db_update_from_server <- function(
    token,
    path_data = data_path(),
    redcap_info = c("record_id", "redcap_data_access_group"),
    file_name = "tipnet.rds"
) {

    file_path <- file.path(path_data, file_name)

    raw <- read_redcap(study_redcap_url(), token)
    all_ok <- raw$data$success && raw$meta_data$success

    if (all_ok) {
        meta <- tidy_extract(raw, "meta")

        study <- raw %>%
            tidy_extract("data") %>%
            nest_tables(redcap_info = redcap_info) %>%
            dplyr::mutate(
                tables = purrr::map2(
                    .data$sheet, .data$tables, factorize_sheet,
                    meta = meta
                )
            )

        readr::write_rds(tipnet, path = file_path)

        return(invisible(file_path))

    } else {
        ui_warn("Connection error to REDCap (in server-start chunk).")
        return(invisible(file_path))
    }

}
