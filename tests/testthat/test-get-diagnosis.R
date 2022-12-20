library(testthat)
test_that(
  "The get_diag() errors when no subject_id_col, diag_col,
                         treat_col",
  {
    data(iris)
    expect_error(get_diag(iris, "SUBJID", "DIAGTYPE", "ATRT"))
  }
)

test_that(
  "The get_diag() errors when diag_col is provided in a wrong
  type",
  {
    data(full_data)
    l <-  (get_diag(full_data, "SUBJID", "AGE", "ATRT"))
    expect_true(identical(l,
                "Wrong input type,diag_col should be character type"))
  }
)


test_that(
  "The get_diag() errors when diag_stage_col is provided in a wrong
  type",
  {
    data(full_data)
    l <-  (get_diag(full_data, "SUBJID", "DIAGTYPE", "ATRT", "AGE"))
    expect_true(identical(l,
            "Wrong input type, diag_stage_col should be character type"))
  }
)


test_that(
  "The get_diag() returns a list.",
  {
    data(full_data)
    l <-  (get_diag(full_data, "SUBJID", "DIAGTYPE", "ATRT"))
    expect_true(inherits(l, "list"))
  }
)


test_that(
  "The first element of the returned list from get_diag() is
  a data.frame object.",
  {
    data(full_data)
    d <-  (get_diag(full_data, "SUBJID", "DIAGTYPE", "ATRT", "DIAGSTAG"))[[1]]
    expect_true(inherits(d, "data.frame"))
  }
)


test_that(
  "The second element of the returned list from get_diag() is
  a ggplot object.",
  {
    data(full_data)
    p <-   get_diag(full_data, "SUBJID", "DIAGTYPE", "ATRT", "DIAGSTAG")[[2]]
    expect_true(inherits(p, "gg"))
  }
)


test_that(
  "The get_diag() returns the right table when only the column
  contains diagnosis type is provided.",
  {
    data(full_data)
    test <- full_data
    test[which(is.na(unlist(full_data[, "DIAGTYPE"]))),
         "DIAGTYPE"] <- "Unknown"
    test[which(full_data[, "DIAGTYPE"] == ""), "DIAGTYPE"] <- "Unknown"
    expect <- test %>%
      select(SUBJID, DIAGTYPE, ATRT) %>%
      distinct() %>%
      group_by(DIAGTYPE, ATRT) %>%
      dplyr::summarize(n = dplyr::n(), .groups = "drop")

    d <-  get_diag(full_data, "SUBJID", "DIAGTYPE", "ATRT")[[1]]
    expect_true(identical(expect, d))
  }
)

test_that(
  "The get_diag() returns the right plot without diagnosis stage provided.",
  {
    data(full_data)

    p <-  get_diag(full_data[1:1000, ], "SUBJID", "DIAGTYPE", "ATRT")[[2]]
    vdiffr::expect_doppelganger("first-1000-diag-without-stage", p)
  }
)





test_that(
  "The get_response() returns the right table when the additional
  diagnosis state is provided.",
  {
    data(full_data)
    test <- full_data
    test[is.na(unlist(test[, "DIAGTYPE"])), "DIAGTYPE"] <- "Unknown"
    test[which(test[, "DIAGTYPE"] == ""), "DIAGTYPE"] <- "Unknown"
    test[is.na(unlist(test[, "DIAGSTAG"])), "DIAGSTAG"] <- "Unknown"
    test[which(test[, "DIAGSTAG"] == ""), "DIAGSTAG"] <- "Unknown"
    expect <- test %>%
      select("SUBJID", "DIAGTYPE", "ATRT", "DIAGSTAG") %>%
      distinct() %>%
      group_by(DIAGTYPE, DIAGSTAG, ATRT) %>%
      dplyr::summarize(n = dplyr::n(), .groups = "drop")

    d <-  get_diag(full_data, "SUBJID", "DIAGTYPE", "ATRT", "DIAGSTAG")[[1]]
    expect_true(identical(expect, d))
  }
)


test_that(
  "The get_diag() returns the right plot with diagnosis stage provided.",
  {
    data(full_data)
    p <-  get_diag(full_data, "SUBJID", "DIAGTYPE", "ATRT", "DIAGSTAG")[[2]]
    vdiffr::expect_doppelganger("first-1000-diag-with stage", p)
  }
)
