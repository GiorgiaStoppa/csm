#' Select the data of a center
#'
#' The function filters the rows of the nested tables returned by
#' [nest_tables] that correspond to a specific center.
#'
#' @param nested_tables (data.frame) the output of the function
#'                                   [nest_tables]
#' @param center_id (character) the name of the column that identifies
#'                              the centers.
#' @param center_label (character) the label of the center.
#'
#' @return the nested tables [tibble][tibble::tibble-package] with the
#'         data of just one center.
#'
#' @details The function aims to retrieve one dataset for each center.
#'          The single dataset will be used as basis to implement
#'          the monitor of data quality.
#' @export
#'
#' @examples
#'
#'nested_tab <- tibble::tibble(
#'    fields = c(rep("demo_clinical", 3L), rep("follow_up", 3L)),
#'    sheets = c(
#'        "demo", "risk_factors", "clinical", "discharge",
#'        "month_1", "year_1"
#'    ),
#'    tables = purrr::map(
#'        .x = seq_len(6),
#'        ~ tibble::tibble(
#'            id = rep(glue::glue("id_{1:5}"), 2L),
#'            center = rep(c("center_1", "center_2"), each = 5L),
#'            x = c(0.1, 0.2, 0.15, 0.4, 0.5, 0.6, 0.3, 0.2, 0.2, 0.1),
#'            y = c(0.1, 0.11, 0.12, 0.3, 0.3, 0.3, 0.4, 0.5, 0.6, 0.55),
#'            z = c(0.1, 0.2, 0.15, 0.4, 0.5, 0.6, 0.3, 0.2, 0.2, 0.1)
#'        )
#'    )
#')
#'
#'select_center(nested_tab, "center", "center_1")
#'

select_center <- function(
    nested_tables, center_id = "center", center_label
) {

    assertive::assert_is_data.frame(nested_tables)
    assertive::assert_is_character(center_id)
    assertive::assert_is_character(center_label)

    nested_tables %>%
        dplyr::mutate(
            tables = purrr::map(
                .x = .data[["tables"]],
                ~ .x %>%
                    dplyr::filter(.data[[center_id]] == center_label)
            )
        )

}
