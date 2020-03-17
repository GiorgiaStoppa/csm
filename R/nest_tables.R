#' Nest the tables of REDCap db
#'
#' Create a tibble with nested tables for each field and its associated
#' sheets.
#'
#' @param data (list) the output of [read_redcap].
#'
#' @param redcap_info (character) the names of the variables that
#'                                identify the subject and the site
#'                                where the subject was enrolled.
#'
#' @return [tibble][tibble::tibble-package] a tibble that contains a
#'                                          column that identifies the
#'                                          fields, a column that
#'                                          identifies the sheets and
#'                                          a column with list of
#'                                          tibbles associated to each
#'                                          field and sheet.
#' @export
#'
#' @examples
#' library(tibble)
#'
#' dd <- tibble(
#'   id = c(rep("1", 2L), rep("2", 2L), rep("3", 2L)),
#'   site = c(rep("AA", 4L), rep("BB", 2L)),
#'   fields = rep(c("demo and clinical", "discharge"), 3L),
#'   sex = c(
#'     "female", NA_character_,
#'     "male", NA_character_,
#'     "female", NA_character_
#'   ),
#'   age = c(
#'     44, NA_real_,
#'     50, NA_real_,
#'     71, NA_real_
#'   ),
#'   demo_complete = c(
#'     "complete", NA_character_,
#'     "complete", NA_character_,
#'     "complete", NA_character_
#'   ),
#'   hp = c(
#'     "yes", NA_character_,
#'     "no", NA_character_,
#'     "yes", NA_character_
#'   ),
#'   diabetes = c(
#'     "no", NA_character_,
#'     "no", NA_character_,
#'     "yes", NA_character_
#'   ),
#'   clinical_complete = c(
#'     "complete", NA_character_,
#'     "complete", NA_character_,
#'     "complete", NA_character_
#'   ),
#'   fe = c(
#'     NA_real_, 35,
#'     NA_real_, 56,
#'     NA_real_, 44
#'   ),
#'   death = c(
#'     NA_character_, "no",
#'     NA_character_, "no",
#'     NA_character_, "yes"
#'   ),
#'   discharge_complete = c(
#'     NA_character_, "complete",
#'     NA_character_, "complete",
#'     NA_character_, "complete"
#'   )
#' )
#'
#' nest_tables(data = dd, redcap_info = c("id", "site"))

nest_tables <- function(data, redcap_info) {

  assertive::assert_is_data.frame(data)
  assertive::assert_is_character(redcap_info)

  data %>%
      tidyr::nest(tables = -.data$fields) %>%
      dplyr::mutate(
          tables = purrr::map(.data$tables, ~{
                  janitor::remove_empty(.x, c("rows", "cols")) %>%
                  add_sheets_prefix(exept = redcap_info) %>%
                  sheets_to_var("sheets", redcap_info) %>%
                  tidyr::nest(tables = -.data$sheets)
          })
      ) %>%
      tidyr::unnest(.data$tables)
}

# Make the sheets a variable in the nested tibble ----------------------
sheets_to_var <- function(data, name, exept) {

  assertive::assert_is_data.frame(data)
  assertive::assert_is_character(name)
  assertive::assert_is_character(exept)

  names_pattern <- paste0(
      "(", paste(attr(data, 'sheet_names'), collapse = '|'), ")",
      "_(.*)"
  )

  unused <- setdiff(exept, names(data))
  cols_to_exclude <- setdiff(exept, unused)

  tidyr::pivot_longer(
      data = data,
      cols = -tidyselect::all_of(cols_to_exclude),
      names_to = c(name, ".value"),
      names_pattern = names_pattern
  )
}

