test_that(
  "The plot_age() errors when no subject_id_col, age_col,
  treat_col",
  {
    data(iris)
    expect_error(plot_age(iris, "SUBJID", "AGE", "ATRT", 10))
  }
)


test_that(
  "The plot_age() returns a ggplot object.",
  {
    data(full_data)
    p <-  plot_age(full_data[1:1000, ], "SUBJID", "AGE", "ATRT", 10)
    expect_true(inherits(p, "gg"))
  }
)

test_that(
  "The plot_age() works on numerical age variables",
  {
    data(full_data)
    p <-  plot_age(full_data[1:1000, ], "SUBJID", "AGE", "ATRT", 10)
    vdiffr::expect_doppelganger("first-1000-ages-distribution", p)
  }
)


test_that(
  "The  plot_age() works on categorical age variables with type factor",
  {
    data(full_data)

    test <- full_data
    test$test_agecol <- as.factor(ifelse(test$AGE > 65, yes = "Older than 65",
                                  no = "Younger than 65"))

    p <-  plot_age(test, "SUBJID", "test_agecol", "ATRT", 10)
    vdiffr::expect_doppelganger("factor-age-distribution", p)
  }
)


test_that(
  "The plot_age() works on categorical age variables with type character",
  {
    data(full_data)

    test <- full_data
    test$test_agecol <- ifelse(test$AGE > 65, yes = "Older than 65",
                                            no = "Younger than 65")

    p <-  plot_age(test, "SUBJID", "test_agecol", "ATRT", 10)
    vdiffr::expect_doppelganger("character-age-distribution", p)
  }
)

test_that(
  "The plot_age() changes column width by modifying age_interval
  when working with numerical values",
  {
    data(full_data)

    p <-  plot_age(full_data[1:1000, ], "SUBJID", "AGE", "ATRT", 5)
    vdiffr::expect_doppelganger("first-1000-ages-interval-5", p)
  }
)
