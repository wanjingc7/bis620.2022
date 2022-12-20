#' Summarize and Plot Distribution of a Selected Variable
#' in Clinical Trials Data
#' This function summarizes and plots distribution of a selected categorical
#' variable in each of the study arm(s).
#' @param df an object inherited from data.frame. It is
#' assumed to have a column containing participants' subject ids, a column
#' containing the selected variable's information (should be categorical),
#' a column containing study arm information.
#' @param subject_id_col the name of the column that contains
#' participants' id information
#' @param var_col the name of the column that contains the variable of
#' interest
#' @param treat_col the name of the column that contains the study arm
#' information
#' @return a list containing the following two objects:
#' 1. a table with distribution of variable of interest in each study arm
#' 2. a bar chart presenting distribution of variable of interest with
#' study arms shown as side-by-side bars.
#' @importFrom dplyr select group_by summarize distinct
#' @importFrom ggplot2 aes ggplot geom_col theme theme_minimal
#' labs geom_text lims
#' @importFrom tidyr %>%
#' @importFrom stats reorder
#' @examples
#' data(full_data)
#' descriptive_var(full_data, "SUBJID", "B_ECOG", "ATRT")
#' @export

descriptive_var <- function(df, subject_id_col, var_col, treat_col) {

  var_col_symbol <- as.symbol(var_col)
  subject_id_symbol <- as.symbol(subject_id_col)
  treat_col_symbol <- as.symbol(treat_col)


  if (is.character(unlist(df[, var_col]))) {

    df[which(is.na(unlist(df[, var_col]))), var_col] <- "Unknown"
    df[which(df[, var_col] == ""), var_col] <- "Unknown"

    var_distribution <- df %>%
      select({{subject_id_symbol}}, {{var_col_symbol}},
             {{treat_col_symbol}}) %>%
      distinct() %>%
      group_by({{var_col_symbol}}, {{treat_col_symbol}}) %>%
      dplyr::summarize(n = n(), .groups = "drop")


    plt <- ggplot(var_distribution,
                  aes(x = reorder({{var_col_symbol}}, -n),
                      y = n, fill = {{treat_col_symbol}})) +
      geom_col(position = "dodge") +
      theme_minimal() +
      ylim(c(0, max(var_distribution$n + 10))) +
      labs(x = var_col, y = "Count") +
      geom_text(aes(label = n), position = position_dodge(.8),
                vjust = 0) +
      ggtitle(bquote(paste("Bar chart of " * .(var_col),
                           " distribution by study arms"))) +
      theme(plot.title = element_text(hjust = 0.5))


    ret <- list(var_distribution, plt)
  }else {
    ret <- paste(c("Wrong input type, var_col",
                   "should be character type"), collapse = " ")
  }

  return(ret)
}
