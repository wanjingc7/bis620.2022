#' Summarize and Plot Distribution of Diagnosis from Clinical Trials Data
#' This function summarizes and plots distribution of diagnosis (and stages)
#' in each of the study arm(s).
#' @param df an object inherited from data.frame. It is
#' assumed to have a column containing participants' subject ids, a column
#' containing the primary diagnosis (should be categorical),
#' a column containing study arm information.
#' @param subject_id_col the name of the column that contains
#' participants' id information
#' @param diag_col the name of the column that contains the primary diagnosis
#' @param treat_col the name of the column that contains the study arm
#' information
#' @param diag_stage_col Optional. The name of the column that contains
#' information about the stage of the disease.Optional (default `NULL`)
#' @return a list containing the following two objects:
#' 1. a table with distribution of the diagnosis (or diagnosis and stage)
#' in each study arm
#' 2. a histogram presenting distribution of diagnosis with study arms shown
#' as side-by-side bars (if stage not provided), or a a histogram presenting
#' distribution of diagnosis and stages faceted by study arms (stage provided)
#' @importFrom dplyr select group_by summarize ungroup distinct vars
#' @importFrom ggplot2 aes ggplot geom_col theme_minimal labs geom_text
#' facet_wrap
#' @importFrom tidyr %>%
#' @importFrom stats reorder
#' @examples
#' data(full_data)
#' get_diag(full_data, "SUBJID", "DIAGTYPE", "ATRT", "DIAGSTAG") #with stages
#' get_diag(full_data, "SUBJID", "DIAGTYPE", "ATRT") #without stages
#' @export


get_diag <- function(df, subject_id_col, diag_col, treat_col,
                     diag_stage_col = NULL) {

  diag_col_symbol <- as.symbol(diag_col)
  subject_id_symbol <- as.symbol(subject_id_col)
  treat_symbol <- as.symbol(treat_col)


  if (is.character(unlist(df[, diag_col]))) {
    df[which(is.na(df[, diag_col])), diag_col] <- "Unknown"
    df[which(df[, diag_col] == ""), diag_col] <- "Unknown"
  }else {
    ret <- paste(c("Wrong input type,diag_col",
                   "should be character type"),
                 collapse = " ")
    return(ret)
  }

  if (is.null(diag_stage_col)) {

    diag_distribution <- df %>%
      select({{subject_id_symbol}}, {{diag_col_symbol}},
             {{treat_symbol}}) %>%
      distinct() %>%
      group_by({{diag_col_symbol}}, {{treat_symbol}}) %>%
      dplyr::summarize(n = dplyr::n(), .groups = "drop")

    plt <- ggplot(diag_distribution,
                  aes(x = reorder({{diag_col_symbol}}, -n),
                      y = n, fill = {{treat_symbol}})) +
      geom_col(position = "dodge") +
      theme_minimal() +
      labs(x = diag_col, y = "Count") +
      geom_text(aes(label = n), position = position_dodge(.8),
                vjust = 0, size = 4)

    ret <- list(diag_distribution, plt)
  }else {
    diag_stage_symbol <- as.symbol(diag_stage_col)

    if (is.character(unlist(df[, diag_stage_col]))) {
      df[which(is.na(unlist(df[, diag_stage_col]))),
         diag_stage_col] <- "Unknown"
      df[which(df[, diag_stage_col] == ""), diag_stage_col] <- "Unknown"
    }else {
      ret <- paste(c("Wrong input type, diag_stage_col",
                     "should be character type"),
                   collapse = " ")
      return(ret)
    }

    diag_distribution <- df %>%
      select({{subject_id_symbol}}, {{diag_col_symbol}},
             {{diag_stage_symbol}}, {{treat_symbol}}) %>%
      distinct() %>%
      group_by({{diag_col_symbol}}, {{diag_stage_symbol}},
               {{treat_symbol}}) %>%
      dplyr::summarize(n = n(), .groups = "drop")


    plt <- ggplot(diag_distribution,
                  aes(x = reorder({{diag_col_symbol}}, -n),
                      y = n, fill = {{diag_stage_symbol}})) +
      geom_col(position = "dodge") +
      labs(x = diag_col, y = "Count") +
      geom_text(aes(label = n), position = position_dodge(.8),
                vjust = 0, size = 4) +
      facet_wrap(vars({{treat_symbol}}))

    ret <- list(diag_distribution, plt)

  }
  return(ret)
}
