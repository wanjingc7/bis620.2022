#' Summarize and Plot Distribution of Adverse Events in Clinical Trials Data
#' This function summarizes and plots distribution of adverse events in each
#' of the study arm(s).
#' @param df an object inherited from data.frame. It is
#' assumed to have a column containing participants' subject ids, a column
#' containing the selected variable's information (should be categorical),
#' a column containing study arm information.
#' @param ae_col the name of the column that contains adverse events
#' @param treat_col the name of the column that contains the study arm
#' information
#' @param top a numeric value represents the number of most frequent adverse
#' events to be shown in the table and the plot, default `20`
#' @return a list containing the following two objects:
#' 1. a table with distribution of adverse events in each study arm
#' 2. a plot presenting distribution of adverse events, faceted by study
#' arms
#' @importFrom dplyr select group_by summarize distinct vars
#' @importFrom ggplot2 aes ggplot geom_col theme_bw theme labs geom_text
#' @importFrom tidyr pivot_wider
#' @importFrom stats reorder
#' @importFrom tidyr %>%
#' @examples
#' data(full_data)
#' get_ae(full_data, "AETERM", "ATRT", 20)
#' @export


get_ae <- function(df, ae_col, treat_col, top = 20) {

  treat_col_symbol <- as.symbol(treat_col)
  ae_col_symbol <- as.symbol(ae_col)

  ae <- df %>%
    select({{ae_col_symbol}}, {{treat_col_symbol}}) %>%
    group_by({{treat_col_symbol}}, {{ae_col_symbol}}) %>%
    dplyr::summarize(count = n(), .groups = "drop")

  ae_plot <- ae %>%
    group_by({{treat_col_symbol}}) %>%
    dplyr::slice_max(order_by = count, n = top)

  ae_ret <- ae %>%
    group_by({{treat_col_symbol}}) %>%
    dplyr::slice_max(order_by = count, n = top) %>%
    pivot_wider(names_from = treat_col, values_from = count)

  plt <- ggplot(ae_plot, aes(x = reorder({{ae_col_symbol}}, -count),
                             y = count)) +
    geom_col() +
    theme_bw() +
    xlab("Adverse Events") +
    ylab("Count") +
    geom_text(aes(label = count), vjust = 0, size = 2.5) +
    ggtitle("Distribution of Adverse Events Faceted by Study Arms") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    facet_wrap(vars({{treat_col_symbol}}), scales = "free_x")

  ret <- list(ae_ret, plt)
  return(ret)
}
