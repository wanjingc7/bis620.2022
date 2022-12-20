## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(bis620.2022)

## ---- include = FALSE---------------------------------------------------------
library(knitr)
library(dplyr)

## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


## -----------------------------------------------------------------------------
#> load full_data table
data("full_data")

#> a brief overview of the first patient from the demo dataset
full_data[1, ] %>% kable()

## ---- fig.align='center', fig.dim = c(5, 3.5)---------------------------------
bis620.2022::plot_age(full_data, "SUBJID", "AGE", "ATRT", 10)

## ---- fig.align='center', fig.dim = c(5, 3.5)---------------------------------
bis620.2022::descriptive_var(full_data, "SUBJID", "SEX", "ATRT")[[1]] %>%
  kable()
print(bis620.2022::descriptive_var(full_data, "SUBJID", "SEX", "ATRT")[[2]])

## ---- fig.align='center', fig.dim = c(10, 3)----------------------------------
descriptive_var(full_data, "SUBJID", "RACE", "ATRT")[[1]] %>%
  kable()
print(descriptive_var(full_data, "SUBJID", "RACE", "ATRT")[[2]])

## ---- fig.align='center', fig.dim = c(5, 3.5)---------------------------------
descriptive_var(full_data, "SUBJID", "HPV", "ATRT")[[1]] %>%
  kable()
print(descriptive_var(full_data, "SUBJID", "HPV", "ATRT")[[2]])

## ---- fig.align='center', fig.dim = c(7, 3.5)---------------------------------
descriptive_var(full_data, "SUBJID", "B_ECOG", "ATRT")[[1]] %>%
  kable()
print(descriptive_var(full_data, "SUBJID", "B_ECOG", "ATRT")[[2]])

## ----fig.align='center', fig.dim = c(10, 3)-----------------------------------
get_diag(full_data, "SUBJID", "DIAGTYPE", "ATRT")[[1]] %>%
  head(10) %>% 
  kable()#the first 10 rows of the counts of diagnosis types without stages are shown below
print(get_diag(full_data, "SUBJID", "DIAGTYPE", "ATRT")[[2]])

## ---- fig.align='center', fig.dim = c(10, 3)----------------------------------
get_diag(full_data, "SUBJID", "DIAGTYPE", "ATRT", "DIAGSTAG")[[1]] %>%
  head(10) %>% 
  kable()#the first 10 rows of the counts of diagnosis types with stages are shown below
print(get_diag(full_data, "SUBJID", "DIAGTYPE", "ATRT", "DIAGSTAG")[[2]])

## -----------------------------------------------------------------------------
l1 <- prognostic_var(full_data, "SUBJID", "ATRT", "Chemotherapy",
                     "DEATH_FLAG", "DIAGTYPE")
var_coef <- l1$coefficients
p_values <- summary(l1)$coefficients[, "Pr(>|z|)"]

as.data.frame(cbind(var_coef, p_values)) %>% kable()

## -----------------------------------------------------------------------------
l2 <- prognostic_var(full_data, "SUBJID", "ATRT", "Chemotherapy",
                     "DEATH_FLAG", "B_ECOG")
var_coef <- l2$coef
p_values <- summary(l2)$coefficients[, "Pr(>|z|)"]
as.data.frame(cbind(var_coef, p_values)) %>% kable()

## -----------------------------------------------------------------------------
l3 <- predictive_var(full_data, "SUBJID", "DEATH_FLAG", "ATRT",
                       var_col = "SEX", interaction = TRUE)
var_coef <- l3$coefficients
p_values <- summary(l3)$coefficients[, "Pr(>|z|)"]

as.data.frame(cbind(var_coef, p_values)) %>% kable()

## -----------------------------------------------------------------------------
l4 <- predictive_var(full_data, "SUBJID", "DEATH_FLAG", "ATRT",
                       var_col = "AGE", interaction = TRUE)
var_coef <- l4$coefficients
p_values <- summary(l4)$coefficients[, "Pr(>|z|)"]
as.data.frame(cbind(var_coef, p_values)) %>% kable()

## -----------------------------------------------------------------------------
l5 <- predictive_var(full_data, "SUBJID", "DEATH_FLAG", "ATRT",
                       var_col = "RACE", interaction = TRUE)

var_coef <- l5$coefficients
p_values <- summary(l5)$coefficients[, "Pr(>|z|)"]
as.data.frame(cbind(var_coef, p_values)) %>% kable()

## -----------------------------------------------------------------------------
l6 <- predictive_var(full_data, "SUBJID", "DEATH_FLAG", "ATRT",
                       var_col = "HPV", interaction = TRUE)

var_coef <- l6$coefficients
p_values <- summary(l6)$coefficients[, "Pr(>|z|)"]
as.data.frame(cbind(var_coef, p_values)) %>% kable()

## -----------------------------------------------------------------------------
l7 <- predictive_var(full_data, "SUBJID", "DEATH_FLAG", "ATRT",
                       var_col = "B_ECOG", interaction = TRUE)

var_coef <- l7$coefficients
p_values <- summary(l7)$coefficients[, "Pr(>|z|)"]
as.data.frame(cbind(var_coef, p_values)) %>% kable()

## -----------------------------------------------------------------------------
l8 <- predictive_var(full_data, "SUBJID", "DEATH_FLAG", "ATRT",
                       var_col = "DIAGTYPE", interaction = TRUE)

var_coef <- l8$coefficients
p_values <- summary(l8)$coefficients[, "Pr(>|z|)"]
as.data.frame(cbind(var_coef, p_values)) %>% kable()

## ---- fig.align='center', fig.dim = c(10, 3)----------------------------------
get_response(full_data, "SUBJID", "ATRT", "RSRESP", "RSCONFYN", "DIAGTYPE")[[1]]%>%
   kable()

print(get_response(full_data, "SUBJID", "ATRT", "RSRESP", "RSCONFYN", "DIAGTYPE")[[2]])

## ---- fig.align='center', fig.dim = c(10, 3), warning=FALSE-------------------
get_ae(full_data, "AETERM", "ATRT", 20)[[1]] %>%
  kable()
print(get_ae(full_data, "AETERM", "ATRT", 20)[[2]])

