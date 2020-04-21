#' Join the nested tables
#'
#' The functions performs a multiple `left_join` of the output from
#' [nest_tables] using [plyr::join_all]. The join is
#'  performed using the IDs from the center and the subjects.
#'
#' @param nested_tables (data.frame) the output from [nest_tables]
#' @param redcap_info (character) the names of the variables that
#'                                identify the subject and the site
#'                                where the subject was enrolled.
#'
#' @return [tibble][tibble::tibble-package] a tibble with all the fields
#'                                          of the study identified by
#'                                          a unique column name.
#'
#' @details The functions has been implemented to create a unique
#'          data.frame with all the fields included in the eCRF of the
#'          study. The data.frame is built by performing a multiple
#'          `left_join` of the output from [nest_tables] using
#'          [plyr::join_all]. Moreover, all the columns
#'          are guaranteed to have a unique name by adding the union
#'          of fields and sheets names as suffix.
#'
#' @export
#'
#' @examples
#'
#' nested_tab <- tibble::tibble(
#'     fields = c(rep("demo_clinical", 3L), rep("follow_up", 3L)),
#'     sheets = c(
#'         "demo", "risk_factors", "clinical", "discharge",
#'         "month_1", "year_1"
#'     ),
#'     tables = purrr::map(
#'         .x = seq_len(6),
#'         ~ tibble::tibble(
#'             id = rep(glue::glue("id_{1:5}"), 2L),
#'             center = rep(c("center_1", "center_2"), each = 5L),
#'             x = c(0.1, 0.2, 0.15, 0.4, 0.5, 0.6, 0.3, 0.2, 0.2, 0.1),
#'             y = c(0.1, 0.11, 0.12, 0.3, 0.3, 0.3, 0.4, 0.5, 0.6, 0.55),
#'             z = c(0.1, 0.2, 0.15, 0.4, 0.5, 0.6, 0.3, 0.2, 0.2, 0.1)
#'         )
#'     )
#' )
#'
#' join_tables(nested_tab, c("id", "center"))
#'

join_tables <- function(
    nested_tables, redcap_info = c("record_id", "center")
) {

    assertive::assert_is_data.frame(nested_tables)
    assertive::assert_is_character(redcap_info)

    nested_tab_list <- nested_tables %>%
        # Remove with spaces with _ in fields
        dplyr::mutate(
            fields = stringr::str_replace_all(
                .data[["fields"]], pattern = " ", replacement = "_"
            )
        ) %>%
        # Add suffix for columns names
        tidyr::unite("suffix", -.data[["tables"]], sep = "_") %>%
        dplyr::mutate(
            tables = purrr::map2(
                .x = .data[["tables"]], .y = .data[["suffix"]],
                ~ add_table_suffix(
                    data = .x, suffix = .y, except = redcap_info
                )
            )
        )

    tab_list <- nested_tab_list[["tables"]]

    purrr::reduce(tab_list, dplyr::left_join, by = redcap_info) %>%
        janitor::remove_empty(c("cols", "rows"))

}
