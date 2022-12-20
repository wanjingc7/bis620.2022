#' Summarize and Plot Distribution of Best Response in Clinical Trials Data
#' This function summarizes and plots distribution of best response throughout
#' the treatment of a participant in each of the study arm(s) and selected
#' groups(optional).
#' @param df an object inherited from data.frame. It is
#' assumed to have a column containing participants' subject ids, a column
#' containing the selected variable's information (should be categorical),
#' a column containing study arm information.
#' @param subject_id_col the name of the column that contains
#' participants' id information
#' @param treat_col the name of the column that contains the study arm
#' information
#' @param response_col the name of the column that contains the response
#' to treatments. The column should contain only character types with
#' the values limited to `"Progressive disease"`, `"Stable disease"`,
#' `"Partial response"`, `"Complete response"` in a case-insensitive form.
#' Leading and Trailing spaces are accepted
#' @param response_conf_col Optional. The name of the column that contains
#' information on whether the response is confirmed. Only `Y` values will be
#' considered as confirmed.If not provided, all responses in `response_col`
#' will be included. Default to `NULL`.
#' @param group_col Optional. The name of the column that contains grouping
#' information for counting. Can only be categorical with character type. If
#'  provided, responses levels will be counted within each unique group.
#'  Default to `NULL`.
#' @return a list containing the following two objects:
#' 1. a table with distribution of best response in each study arm
#' 2. a plot presenting distribution of best response, faceted by study
#' arms (if no `group_col` provided), or a plot presenting distribution of
#' best response, faceted by study arms and groups (if `group_col` provided)
#' @importFrom dplyr select group_by summarize distinct vars n mutate slice_max
#' slice case_when filter
#' @importFrom ggplot2 aes ggplot geom_col theme theme_bw labs geom_text
#' facet_wrap scale_x_discrete xlab ylab labs ggtitle lims ylim position_dodge
#' @importFrom tidyr pivot_wider as_tibble
#' @importFrom tidyr %>%
#' @examples
#' data(full_data)
#' get_response(full_data, "SUBJID", "ATRT", "RSRESP", "RSCONFYN",
#' "DIAGTYPE") #both `response_conf_col` and `group_col` given
#' get_response(full_data, "SUBJID", "ATRT", "RSRESP", "RSCONFYN") #only
#' #`response_conf_col`
#' get_response(df = full_data, subject_id_col = "SUBJID",
#' treat_col = "ATRT", response_col = "RSRESP", group_col = "DIAGTYPE")
#' #only `group_col` provided
#' get_response(df = full_data, subject_id_col = "SUBJID",
#' treat_col = "ATRT", response_col = "RSRESP") #no `response_conf_col`
#' #or `group_col`.
#' @export




get_response <- function(df, subject_id_col, treat_col,
                         response_col, response_conf_col = NULL,
                         group_col = NULL) {

  subject_id_symbol <- as.symbol(subject_id_col)
  treat_col_symbol <- as.symbol(treat_col)
  response_col_symbol <- as.symbol(response_col)
  response_conf_symbol <- ifelse(is.null(response_conf_col),
                                 yes = "Y",
                                 no = as.symbol(response_conf_col))
  group_col_symbol <-  ifelse(is.null(group_col),
                              yes = TRUE,
                              no = as.symbol(group_col))

  if(!is.character(unlist(df[, response_col]))){
    return("Wrong input type, response_col should be character type")
  }

  if(!is.null(response_conf_col)){
    if(!is.character(unlist(df[, response_conf_col]))){
      return("Wrong input type, response_conf_col should be character type")
    }
  }

  if(!is.null(group_col)){
    if(is.character(unlist(df[, group_col]))){
    df[which(is.na(df[, group_col])), group_col] <- "Unknown"
    df[which(df[, group_col] == ""), group_col] <- "Unknown"
    }
    else{
      ret <- "Wrong input type for group_col, should be character"
      return (ret)
    }
  }

  df <- df %>%
    dplyr::mutate(response_code = case_when(
      trimws(tolower({{response_col_symbol}})) ==
        "progressive disease" ~ 1,
      trimws(tolower({{response_col_symbol}})) ==
        "stable disease" ~ 2,
      trimws(tolower({{response_col_symbol}})) ==
        "partial response" ~ 3,
      trimws(tolower({{response_col_symbol}})) ==
        "complete response" ~ 4,
      TRUE ~ 0
    ))

  best_response <- df %>%
    {if (is.null(response_conf_col)) filter(., TRUE)
      else filter(., {{response_conf_symbol}} == "Y")
      } %>%
    {if (is.null(group_col)) select(., {{subject_id_symbol}},
                                    {{treat_col_symbol}},
                                    {{response_col_symbol}}, response_code)
      else select(., {{subject_id_symbol}}, {{treat_col_symbol}},
                  {{response_col_symbol}}, response_code,
                  {{group_col_symbol}})
      } %>%
    distinct() %>%
    filter(response_code != 0) %>%
    group_by({{subject_id_symbol}}) %>%
    slice_max(order_by = response_code, n = 1) %>%
    group_by({{treat_col_symbol}}, {{response_col_symbol}},
             {{group_col_symbol}}) %>%
    summarize(n = n(), .groups = "drop") %>%
    group_by({{treat_col_symbol}}, {{group_col_symbol}}) %>%
    dplyr::summarize(
      n = n, {{response_col_symbol}},
      freq = n / sum(n),
      label = paste(n, "\n(", round(freq * 100, 2), "%)",
                   sep = ""),
      .groups = "drop")




  if (is.null(group_col)) {
    tbl_ret <- best_response %>%
      select({{treat_col_symbol}}, {{response_col_symbol}}, n) %>%
      pivot_wider(names_from = c({{treat_col_symbol}}),
                  values_from = c(n))

    plt <- ggplot(best_response, aes(x = {{response_col_symbol}},
                                     y = n)) +
      geom_col() +
      theme_bw() +
      xlab("Best Response") +
      ylab("Count") +
      geom_text(aes(label = label), vjust = 0, size = 2.5) +
      ggtitle("Distribution of Best Outcomes Faceted by Study Arms") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
      theme(plot.title = element_text(hjust = 0.5)) +
      facet_wrap(vars({{treat_col_symbol}})) +
      scale_x_discrete(limits = c("Progressive disease", "Stable disease",
                                  "Partial response", "Complete response")) +
      ylim(c(0, max(best_response$n + 10)))
  } else {

    tbl_ret <- best_response %>%
      select({{treat_col_symbol}}, {{group_col_symbol}},
             {{response_col_symbol}}, n) %>%
      pivot_wider(names_from = c({{treat_col_symbol}}),
                  values_from = c(n))

    plt <- ggplot(best_response, aes(x = {{response_col_symbol}},
                                     y = n)) +
      geom_col(position = "dodge") +
      theme_bw() +
      xlab("Best Response") +
      ylab("Count") +
      geom_text(aes(label = label), vjust = 0, size = 2.5,
                position = position_dodge(0.8)) +
      ggtitle("Distribution of Best Outcomes Faceted by Study Arms") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
      theme(plot.title = element_text(hjust = 0.5)) +
      facet_grid(rows = vars({{treat_col_symbol}}),
                 cols = vars({{group_col_symbol}})) +
      ylim(c(0, max(best_response$n + 10)))+
      scale_x_discrete(limits = c("Progressive disease", "Stable disease",
                                  "Partial response", "Complete response"))
  }

  ret <- list(tbl_ret, plt)

  return(ret)
}
