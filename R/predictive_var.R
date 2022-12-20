#' Do logistic regression test to see if a Selected Variable
#' in Clinical Trials Data is a moderator of the effect of treatment
#' on the death outcome
#'
#' This function do the logistic regression test for the interaction
#' effect of a selected categorical variable
#' and study arm(s) on the death outcome
#' @param df an object inherited from data.frame. It is
#' assumed to have a column containing participants' subject ids, a column
#' containing the selected variable's information (should be categorical),
#' a column containing study arm information,
#' a column containing death flag information.
#' @param subject_id_col the name of the column that contains
#' participants' id information
#' @param death_col the name of the column that contains survival information.
#' Should be formatted as binary (factor or character)
#' @param treat_col the name of the column that contains the study arm
#' information
#' @param var_col Optional. The name of the column that contains the variable of
#' interest. Should be of type numeric or character. If not provided, this
#' function will analyze the effect of treatment on death only. If provided,
#' this function will analyze the effects of both
#' the selected variable and treatment and whether the selected variable
#' is predictive. Default to `NULL`
#' @param interaction Optional. Whether the interaction effect between treatment
#' and selected variable should be analyzed. Default to `TRUE`.
#' @return a logistic model
#' @importFrom dplyr select group_by summarize distinct
#' @importFrom stats binomial glm
#' @importFrom tidyr %>%
#' @examples
#' data(full_data)
#' predictive_var(full_data, "SUBJID","DEATH_FLAG", "ATRT", "DIAGSTAG", TRUE)
#' @export

predictive_var <- function(df, subject_id_col, death_col, treat_col,
                           var_col = NULL, interaction = TRUE) {


  subject_id_symbol <- as.symbol(subject_id_col)
  treat_col_symbol <- as.symbol(treat_col)
  death_col_symbol <- as.symbol(death_col)

  if (!is.null(var_col)) {
    var_col_symbol <- as.symbol(var_col)
    if (is.character(unlist(df[, var_col]))) {
      df <- df[which(!is.na(unlist(df[, var_col]))), ] %>%
        filter({{var_col_symbol}} != "")

    } else if (is.numeric(unlist(df[, var_col]))) {
      df <- df[which(!is.na(unlist(df[, var_col]))), ]
    } else {
      ret <- paste(c("Wrong input type,", var_col,
                     "should be character or numeric type"), collapse = " ")
      return(ret)
    }

    var_distribution <- df %>%
      select({{subject_id_symbol}}, {{var_col_symbol}},
             {{treat_col_symbol}}, {{death_col_symbol}}) %>%
      distinct()

    death_vector <- unlist(var_distribution[, death_col])
    treat_vector <- unlist(var_distribution[, treat_col])
    var_vector <- unlist(var_distribution[, var_col])

    if (interaction) {
      mylogit <- glm(as.factor(death_vector) ~
                      var_vector * as.factor(treat_vector),
                     family =  binomial(link = "logit"))
    } else {
      mylogit <- glm(as.factor(death_vector) ~
                       var_vector +
                       as.factor(treat_vector),
                     family =  binomial(link = "logit"))
    }
  } else {
    var_distribution <- df %>%
      select({{subject_id_symbol}},
             {{treat_col_symbol}}, {{death_col_symbol}}) %>%
      distinct()

    death_vector <- unlist(var_distribution[, death_col])
    treat_vector <- unlist(var_distribution[, treat_col])
    mylogit <- glm(as.factor(death_vector) ~
                     as.factor(treat_vector),
                   family =  binomial(link = "logit"))
  }
  ret <- mylogit


  return(ret)
}
