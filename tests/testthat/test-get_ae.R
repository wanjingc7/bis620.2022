test_that(
  "The get_ae() errors when no treat_col,
                         ae_col",
  {
    data(iris)
    expect_error(get_ae(iris, "AETERM", "ATRT", 20))
  }
)


test_that(
  "The get_ae() returns a list.",
  {
    data(full_data)
    l <-  get_ae(full_data, "AETERM", "ATRT", 20)
    expect_true(inherits(l, "list"))
  }
)


test_that(
  "The first element of the returned list from get_ae() is
  a data.frame object",
  {
    data(full_data)
    d <-   get_ae(full_data, "AETERM", "ATRT", 20)[[1]]
    expect_true(inherits(d, "data.frame"))
  }
)


test_that(
  "The second element of the returned list from get_ae() is
  a ggplot object",
  {
    data(full_data)
    p <-   get_ae(full_data, "AETERM", "ATRT", 20)[[2]]
    expect_true(inherits(p, "gg"))
  }
)


test_that(
  "The get_ae() returns the right table",
  {
    data(full_data)
    test <- full_data
    expect <- test %>%
      select(AETERM, ATRT) %>%
      group_by(AETERM, ATRT) %>%
      summarize(count = n(), .groups = "drop") %>%
      group_by(ATRT) %>%
      dplyr::slice_max(order_by = count, n = 20) %>%
      pivot_wider(names_from = ATRT, values_from = count)

    d <- get_ae(full_data, "AETERM", "ATRT", 20)[[1]]
    expect_true(identical(expect, d))
  }
)

test_that(
  "The get_response() returns the right plot",
  {
    data(full_data)
    p <- get_ae(full_data, "AETERM", "ATRT", 20)[[2]]
    vdiffr::expect_doppelganger("ae-distribution", p)
  }
)
