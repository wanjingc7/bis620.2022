test_that(
  "The predictive_var() errors when no subject_id_col, diag_col,
                         treat_col.",
  {
    data(iris)
    expect_error(predictive_var(iris, "SUBJID", "DEATH_FLAG",
                                "ATRT", "DIAGSTAG"))
  }
)


test_that(
  "The predictive_var() errors when var_col is provided in the wrong type.",
  {
    data(full_data)
    full_data$SEX <- as.factor(full_data$SEX)
    e <- predictive_var(full_data, "SUBJID", "DEATH_FLAG",
                        "ATRT", "SEX")
    expect_true(identical(
      "Wrong input type, SEX should be character or numeric type",
                e))
  }
)

test_that(
  "The predictive_var() returns an lm object.",
  {
    data(full_data)
    l <- predictive_var(full_data, "SUBJID", "DEATH_FLAG",
                        "ATRT", "HPV")
    expect_true(inherits(l, "lm"))
  }
)

test_that(
  "The predictive_var() returns an lm object.",
  {
    data(full_data)
    l <- predictive_var(full_data, "SUBJID", "DEATH_FLAG",
                        "ATRT", "SEX")
    expect_true(inherits(l, "lm"))
  }
)


test_that(
  "The predictive_var() returns a correct lm object when only treatment
  variable is provided.",
  {
    data(full_data)
    l <- predictive_var(full_data, "SUBJID", "DEATH_FLAG",
                        "ATRT")
    test <- full_data %>%
      filter(!is.na(ATRT)) %>%
      select(SUBJID, DEATH_FLAG, ATRT) %>%
      distinct()

    expect <- glm(as.factor(DEATH_FLAG) ~
                    as.factor(ATRT),
                  data = test,
                  family = binomial(link = "logit"))
    expect_true(identical(l$coefficients, expect$coefficients))
  }
)


test_that(
  "The predictive_var() returns a correct lm object when var_col is character
  and interaction = FALSE.",
  {
    data(full_data)
    l <- predictive_var(full_data, "SUBJID", "DEATH_FLAG",
                        "ATRT", "DIAGSTAG", FALSE)
    test <- full_data %>%
      filter(!is.na(DIAGSTAG)) %>%
      filter(DIAGSTAG != "") %>%
      select(SUBJID, DEATH_FLAG, ATRT, DIAGSTAG) %>%
      distinct()

    expect <- glm(as.factor(DEATH_FLAG) ~
                    as.factor(ATRT) + DIAGSTAG,
                  data = test,
                  family = binomial(link = "logit"))
    expect_true(identical(l$coefficients, expect$coefficients))
  }
)


test_that(
  "The predictive_var() returns a correct lm object when var_col
  is character type and interaction = TRUE.",
  {
    data(full_data)
    l <- predictive_var(full_data, "SUBJID", "DEATH_FLAG",
                        "ATRT", "DIAGSTAG", TRUE)
    test <- full_data %>%
      filter(!is.na(DIAGSTAG)) %>%
      filter(DIAGSTAG != "") %>%
      select(SUBJID, DEATH_FLAG, ATRT, DIAGSTAG) %>%
      distinct()


    expect <- glm(as.factor(DEATH_FLAG) ~
                    as.factor(ATRT) * DIAGSTAG,
                  data = test,
                  family = binomial(link = "logit"))
    expect_true(identical(l$coefficients, expect$coefficients))
  }
)
test_that(
  "The predictive_var() returns a correct lm object when the var_col
  is of numeric type.",
  {
    data(full_data)
    l <- predictive_var(full_data, "SUBJID", "DEATH_FLAG",
                        "ATRT", "AGE")
    test <- full_data %>%
      filter(!is.na(ATRT)) %>%
      filter(!is.na(AGE)) %>%
      select(SUBJID, DEATH_FLAG, ATRT, AGE) %>%
      distinct()

    expect <- glm(as.factor(DEATH_FLAG) ~
                    as.factor(ATRT) * AGE,
                  data = test,
                  family = binomial(link = "logit"))
    expect_true(all.equal(l$coefficients, expect$coefficients))
  }
)
