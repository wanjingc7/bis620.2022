test_that(
  "The prognostic_var() errors when no subject_id_col, diag_col,
                         treat_col",
  {
    data(iris)
    expect_error(prognostic_var(iris, "SUBJID", "ATRT", "Chemotherapy",
                                "DEATH_FLAG", "HPV"))
  }
)


test_that(
  "The prognostic_var() errors when no subject_id_col, var_col,
  treat_col",
  {
    data(full_data)
    l <- prognostic_var(full_data, "SUBJID", "ATRT", "Chemotherapy",
                   "DEATH_FLAG", c("B_ECOG", "SEX"))
    expect_true(identical(l,
    "Wrong input length in var_col, please provide only one variable"))
  }
)



test_that(
  "The prognostic_var() returns an lm object.",
  {
    data(full_data)
    l <- prognostic_var(full_data, "SUBJID", "ATRT", "Chemotherapy",
                        "DEATH_FLAG", "HPV")
    expect_true(inherits(l, "lm"))
  }
)

test_that(
  "The prognostic_var() returns an lm object.",
  {
    data(full_data)
    l <- prognostic_var(full_data, "SUBJID", "ATRT", "Chemotherapy",
                        "DEATH_FLAG", "SEX")
    expect_true(inherits(l, "lm"))
  }
)

test_that(
  "The prognostic_var() returns an lm object",
  {
    data(full_data)
    l <- prognostic_var(full_data, "SUBJID", "ATRT", "Chemotherapy",
                        "DEATH_FLAG", "HPV")
    expect_true(inherits(l, "lm"))
  }
)

test_that(
  "The prognostic_var() returns an lm object.",
  {
    data(full_data)
    l <- prognostic_var(full_data, "SUBJID", "ATRT", "Chemotherapy",
                        "DEATH_FLAG", "RACE")
    expect_true(inherits(l, "lm"))
  }
)

test_that(
  "The prognostic_var() returns an lm object.",
  {
    data(full_data)
    l <- prognostic_var(full_data, "SUBJID", "ATRT", "Chemotherapy",
                        "DEATH_FLAG", "B_ECOG")
    expect_true(inherits(l, "lm"))
  }
)

test_that(
  "The prognostic_var() returns an lm object",
  {
    data(full_data)
    l <- prognostic_var(full_data, "SUBJID", "ATRT", "Chemotherapy",
                        "DEATH_FLAG", "DIAGTYPE")
    expect_true(inherits(l, "lm"))
  }
)



test_that(
  "The predictive_var() returns a correct lm object when var_col is
  of character type.",
  {
    data(full_data)
    l <- prognostic_var(full_data, "SUBJID", "ATRT", "Chemotherapy",
                        "DEATH_FLAG", "DIAGTYPE")
    test <- full_data %>%
      filter(!is.na(ATRT)) %>%
      filter(!is.na(DIAGTYPE)) %>%
      filter(DIAGTYPE != "") %>%
      filter(ATRT == "Chemotherapy") %>%
      select(SUBJID, DEATH_FLAG, ATRT, DIAGTYPE) %>%
      distinct()


    expect <- glm(as.factor(DEATH_FLAG) ~
                    DIAGTYPE,
                  data = test,
                  family = binomial(link = "logit"))
    expect_true(identical(l$coefficients, expect$coefficients))
  }
)



test_that(
  "The predictive_var() returns a correct lm object when var_col is
  of numeric type.",
  {
    data(full_data)
    l <- prognostic_var(full_data, "SUBJID", "ATRT", "Chemotherapy",
                        "DEATH_FLAG", "AGE")
    test <- full_data %>%
      filter(!is.na(ATRT)) %>%
      filter(!is.na(AGE)) %>%
      filter(DIAGTYPE != "") %>%
      filter(ATRT == "Chemotherapy") %>%
      select(SUBJID, DEATH_FLAG, ATRT, AGE) %>%
      distinct()



    expect <- glm(as.factor(DEATH_FLAG) ~
                    AGE,
                  data = test,
                  family = binomial(link = "logit"))
    expect_true(identical(l$coefficients, expect$coefficients))
  }
)




test_that(
  "The predictive_var() returns errirs when var_col is
  of the wrong type.",
  {
    data(full_data)
    test <- full_data
    test$AGE <- as.factor(test$AGE)
    l <- prognostic_var(test, "SUBJID", "ATRT", "Chemotherapy",
                        "DEATH_FLAG", "AGE")

    expect_true(identical(l,
        "Wrong input type,var_col should be character or numeric type"))
  }
)
