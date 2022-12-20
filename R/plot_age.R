#' Plot Age Distribution in Clinical Trials Data
#' This function plots distribution of participants' ages in
#' each of the study arm(s) separately.
#' @param df an object inherited from data.frame. It is
#' assumed to have a column containing participants' subject ids, a column
#' containing age information (can be categorical or numerical), a column
#' containing study arm information.
#' @param subject_id_col the name of the column that contains
#' participants' id information
#' @param age_col the name of the column that contains the age information
#' @param treat_col the name of the column that contains the study arm
#' information
#' @param age_interval a numerical value that controls the age interval
#' represented by each bar in the histogram, default is `10`
#' @return an age distribution histogram faceted by study arms.
#' @importFrom dplyr select group_by summarize distinct n
#' @importFrom tidyr drop_na
#' @importFrom ggplot2 aes ggplot geom_histogram stat_bin scale_x_continuous
#' facet_wrap theme_minimal ggtitle element_text stat
#' @importFrom tidyr %>%
#' @examples
#' data(full_data)
#' plot_age(full_data, "SUBJID", "AGE", "ATRT", 10)
#' @export

plot_age <- function(df, subject_id_col, age_col,
                     treat_col, age_interval = 10) {

  age_col_symbol <- as.symbol(age_col)
  subject_id_symbol <- as.symbol(subject_id_col)
  treat_col_symbol <- as.symbol(treat_col)

  if ((is.character(unlist(df[, age_col]))) ||
       (is.factor(unlist(df[, age_col])))) {
    age_distribution <- df %>%
      select({{subject_id_symbol}}, {{age_col_symbol}},
             {{treat_col_symbol}}) %>%
      distinct() %>%
      drop_na() %>%
      group_by({{age_col_symbol}}, {{treat_col_symbol}}) %>%
      dplyr::summarize(n =  n(), .groups = "drop")

    plt <- ggplot(age_distribution) +
      geom_col(aes(x = {{age_col_symbol}}, y = n,
                   fill = {{treat_col_symbol}}), position = "dodge") +
      theme_minimal()
  } else {
    age_distribution <- df %>%
      select({{subject_id_symbol}}, {{age_col_symbol}},
             {{treat_col_symbol}}) %>%
      distinct() %>%
      drop_na()

    label_intervals <- seq(floor(min(df[, age_col]) / age_interval) *
                             age_interval,
                           ceiling(max(df[, age_col]) / age_interval) *
                             age_interval,
                           by = age_interval)

    plt <- ggplot(age_distribution, aes(x = {{age_col_symbol}})) +
      geom_histogram(color = "lightblue", breaks = label_intervals) +
      stat_bin(breaks = label_intervals, geom = "text",
               aes(label = stat(count)), vjust = 0, size = 2.5) +
      scale_x_continuous(breaks = label_intervals) +
      theme_minimal() +
      ggtitle("Distribution of Age Faceted by Study Arm") +
      theme(plot.title = element_text(hjust = 0.5)) +
      facet_wrap(vars({{treat_col_symbol}}))
  }


  return(plt)
}
