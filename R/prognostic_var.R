#' Do logistic regression test to see if a Selected Variable
#' in Clinical Trials Data is a prognostic in terms of the death outcome.
#'
#' This function conducts logistic regression test between the death outcome
#' and a selected variable.
#' @param df an object inherited from data.frame. It is
#' assumed to have a column containing participants' subject ids, a column
#' containing the selected variable's information (should be categorical),
#' a column containing study arm information,
#' a column containing death flag information.
#' @param subject_id_col the name of the column that contains
#' participants' id information
#' @param treat_col the name of the column that contains the study arm
#' information
#' @param control_val a string representing the name of the control treatment
#' @param death_col the name of the column that contains survival information.
#' Should be formatted as binary (factor or character)
#' @param var_col the name of the column that contains the variable of
#' interest.Can be of character or numeric type.
#' @return a logistic model
#' @importFrom dplyr select group_by summarize distinct
#' @importFrom stats binomial glm as.formula
#' @importFrom tidyr %>%
#' @examples
#' data(full_data)
#' prognostic_var(full_data, "SUBJID", "ATRT",
#' "Chemotherapy", "DEATH_FLAG", "HPV")
#' @export


prognostic_var <- function(df, subject_id_col, treat_col, control_val,
                           death_col, var_col) {

  subject_id_symbol <- as.symbol(subject_id_col)
  treat_col_symbol <- as.symbol(treat_col)
  death_col_symbol <- as.symbol(death_col)

  if (length(var_col) == 1) {
  var_col_symbol <- as.symbol(var_col)
  }else {
    ret <- paste(c("Wrong input length in var_col,",
                   "please provide only one variable"), collapse = " ")
    return(ret)
  }

  if (is.character(unlist(df[, var_col]))) {
    df <- df[which(!is.na(unlist(df[, var_col]))), ] %>%
      filter({{var_col_symbol}} != "")
  } else if (is.numeric(unlist(df[, var_col]))) {
    df <- df[which(!is.na(unlist(df[, var_col]))), ]
  } else {
    ret <- paste(c("Wrong input type,var_col",
                   "should be character or numeric type"), collapse = " ")
    return(ret)
  }

  control_group <- df %>%
    filter({{treat_col_symbol}} == control_val) %>%
    select({{subject_id_symbol}}, {{death_col_symbol}},
           {{treat_col_symbol}}, {{var_col_symbol}}) %>%
    distinct()


  mylogit <- glm(formula = as.formula(paste("as.factor(", death_col,
                                           ") ~ ", var_col)),
                 data = control_group,
                 family =  binomial(link = "logit"))

  return(mylogit)
}
