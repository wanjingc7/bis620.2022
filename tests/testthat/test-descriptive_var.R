test_that(
  "The descriptive_var() errors when no subject_id_col, var_col,
  treat_col",
  {
    data(iris)
    expect_error(descriptive_var(iris))
  }
)

test_that(
  "The descriptive_var() errors when no subject_id_col, var_col,
  treat_col",
  {
    data(full_data)
    l <- descriptive_var(full_data, "SUBJID", "AGE", "ATRT")
    expect_true(identical(l,
                "Wrong input type, var_col should be character type"))
  }
)

test_that(
  "The descriptive_var() returns a list.",
  {
    data(full_data)
    l <- descriptive_var(full_data, "SUBJID", "B_ECOG", "ATRT")
    expect_true(inherits(l, "list"))
  }
)
test_that(
  "The descriptive_var() returns a list.",
  {
    data(full_data)
    l <- descriptive_var(full_data, "SUBJID", "SEX", "ATRT")
    expect_true(inherits(l, "list"))
  }
)
test_that(
  "The descriptive_var() returns a list.",
  {
    data(full_data)
    l <- descriptive_var(full_data, "SUBJID", "RACE", "ATRT")
    expect_true(inherits(l, "list"))
  }
)
test_that(
  "The descriptive_var() returns a list.",
  {
    data(full_data)
    l <- descriptive_var(full_data, "SUBJID", "HPV", "ATRT")
    expect_true(inherits(l, "list"))
  }
)
test_that(
  "The descriptive_var() returns a list.",
  {
    data(full_data)
    l <- descriptive_var(full_data, "SUBJID", "DIAGTYPE", "ATRT")
    expect_true(inherits(l, "list"))
  }
)

test_that(
  "The descriptive_var() returns contains a ggplot object.",
  {
    data(full_data)
    p <-  descriptive_var(full_data, "SUBJID", "DIAGTYPE", "ATRT")[[2]]
    expect_true(inherits(p, "gg"))
  }
)


test_that(
  "The descriptive_var() returns the correct table",
  {
    data(full_data)
    test <- full_data
    expect <- test %>%
      select(SUBJID, DIAGTYPE, ATRT) %>%
      distinct() %>%
      group_by(DIAGTYPE, ATRT) %>%
      summarize(n = n(), .groups = "drop")

    d <- descriptive_var(full_data, "SUBJID", "DIAGTYPE", "ATRT")[[1]]
    expect_true(identical(expect, d))

  }
)

test_that(
  "The descriptive_var() returns correct plots",
  {
    data(full_data)
    p <- descriptive_var(full_data, "SUBJID", "DIAGTYPE", "ATRT")[[2]]
    vdiffr::expect_doppelganger("diagnosis-type-distribution", p)
  }
)
