#' Plot the completeness of the follow-up
#'
#' The function aims to provide a graphical representation of the
#' completeness of the follow-up.
#'
#' @param data (data.frame) a data.frame that contains the data
#'        of the completeness.
#'
#' @param outcome (character) a string with the name of the variable
#'        in [data] that contains the name of the endpoints.
#'
#' @param total (character) a string with the name of the variable
#'        in [data] that contains the number of subject on which
#'        the completeness is evaluated.
#'
#' @param percentage (character) a string with the name of the variable
#'        in [data] that contains the percentage of completeness
#'        and missingness of the follow-up.
#'
#' @param type (character) a string with the name of the variable
#'        in [data] that contains the type of percentage, i.e.
#'        completeness or missingness.
#'
#' @return a [ggplot2::ggplot] object.
#'
#' @details The function is intended to be used when the report of
#'          data quality check is implemented. It can be useful for
#'          clinicians to check the completeness of follow-up in their
#'          data. The input must be a data.frame with the same structure
#'          as described above.
#'
#' @export
#'
#' @examples
#'
#' data <- tibble::tibble(
#'     outcome = c("death", "death", "mi", "mi"),
#'     total = rep(75, 4L),
#'     type = c(
#'         "completeness", "missingness", "completeness", "missingness"
#'     ),
#'     percentage = c(65.5, 34.5, 98, 2)
#' )
#'
#' ggcomplfup(data, "outcome", "total", "type", "percentage")
#'

ggcomplfup <- function(data, outcome, total, type, percentage) {


    assertive::assert_is_data.frame(data)
    assertive::assert_is_character(outcome)
    assertive::assert_is_character(total)
    assertive::assert_is_character(type)
    assertive::assert_is_character(percentage)

    # Check if some variable's names are wrong
    inps <- c(outcome, total, type, percentage)
    nm <- names(data)

    wrongs <- setdiff(inps, nm)

    if (length(wrongs) > 0) {
        usethis::ui_stop(
            glue::glue("Some inputs are not included in the data. Please verify that 'outcome', 'type', 'total' and 'percentage' were correctly spelled.")
        )
    }

    ggplot2::ggplot(
        data = data,
        mapping = ggplot2::aes_string(
            x = outcome, y = percentage, colour = type, fill = type,
            group = type
        )
    ) +
        ggplot2::geom_bar(stat = "identity", alpha = 0.8) +
        ggplot2::annotate(
            geom = "text",
            x = data[[outcome]],
            y = 105,
            label = data[[total]]
        ) +
        ggplot2::scale_fill_discrete(name = "") +
        ggplot2::scale_color_discrete(name = "") +
        ggplot2::xlab("Endpoint") +
        ggplot2::ylab("%")+
        ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)
        )

}
