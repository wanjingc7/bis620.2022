test_that(
  "The get_response() errors when no subject_id_col, treat_col,
                         response_col",
  {
    data(iris)
    expect_error(get_response(iris, "SUBJID", "ATRT", "RSRESP"))
  }
)

test_that(
  "The get_response() errors when response_col is provided in the
  wrong format",
  {
    data(full_data)
    e <- get_response(full_data, "SUBJID", "ATRT", "AGE", "RSCONFYN",
                               "DIAGTYPE")
    expect_true(identical(e,
          "Wrong input type, response_col should be character type"))
  }
)

test_that(
  "The get_response() errors when grouping column is provided in the
  wrong format",
  {
    data(full_data)
    e <- get_response(full_data, "SUBJID", "ATRT", "RSRESP", "RSCONFYN",
                      "AGE")
    expect_true(identical(e,
            "Wrong input type for group_col, should be character"))
  }
)

test_that(
  "The get_response() errors when response_conf_col is provided in the
  wrong format",
  {
    data(full_data)
    e <- get_response(full_data, "SUBJID", "ATRT", "RSRESP", "AGE",
                      "DIAGTYPE")
    expect_true(identical(e,
  "Wrong input type, response_conf_col should be character type"))
  }
)


test_that(
  "The get_response() returns a list.",
  {
    data(full_data)
    l <-  get_response(full_data, "SUBJID", "ATRT", "RSRESP")
    expect_true(inherits(l, "list"))
  }
)


test_that(
  "The first element of the returned list from get_response() is
  a data.frame object",
  {
    data(full_data)
    d <-   get_response(full_data, "SUBJID", "ATRT", "RSRESP")[[1]]
    expect_true(inherits(d, "data.frame"))
  }
)


test_that(
  "The second element of the returned list from get_response() is
  a ggplot object",
  {
    data(full_data)
    p <-   get_response(full_data, "SUBJID", "ATRT", "RSRESP")[[2]]
    expect_true(inherits(p, "gg"))
  }
)


test_that(
  "The get_response() returns the right table when only the column
  contains whether response was confirmed is provided.",
  {
    data(full_data)
    test <- full_data[which(full_data$RSCONFYN == "Y"),
                      c("SUBJID", "ATRT", "RSRESP")] %>%
      distinct() %>%
      filter(trimws(tolower(RSRESP)) %in% c("progressive disease",
                                            "stable disease",
                                            "partial response",
                                            "complete response")) %>%
      mutate(response_code = case_when(
        trimws(tolower(RSRESP)) ==
          "progressive disease" ~ 1,
        trimws(tolower(RSRESP)) ==
          "stable disease" ~ 2,
        trimws(tolower(RSRESP)) ==
          "partial response" ~ 3,
        trimws(tolower(RSRESP)) ==
          "complete response" ~ 4,
        TRUE ~ 0
      )) %>%
      group_by(SUBJID) %>%
      filter(response_code == max(response_code)) %>%
      ungroup()


    expect_df <- as.data.frame.matrix(table(test$RSRESP, test$ATRT))
    expect_df$RSRESP <- rownames(expect_df)
    rownames(expect_df) <- NULL
    expect_tibble <- as_tibble(expect_df %>% select(RSRESP, Chemotherapy,
                                                    `panit. plus chemotherapy`))


    d <-  get_response(full_data, "SUBJID", "ATRT", "RSRESP", "RSCONFYN")[[1]]
    expect_true(identical(expect_tibble, d))
  }
)

test_that(
  "The get_response() returns the right plot when only the column
  contains whether response was confirmed is provided.",
  {
    data(full_data)

    p <-  get_response(full_data[1:1000, ], "SUBJID", "ATRT", "RSRESP",
                       "RSCONFYN")[[2]]
    vdiffr::expect_doppelganger("first-1000-confirmed-outcome", p)
  }
)




test_that(
  "The get_response() returns the right table when only the additional
  grouping column is provided in the correct format (character).",
  {
    data(full_data)
    test <- full_data[, c("SUBJID", "ATRT", "RSRESP", "B_ECOG")] %>%
      distinct() %>%
      filter(trimws(tolower(RSRESP)) %in% c("progressive disease",
                                            "stable disease",
                                            "partial response",
                                            "complete response")) %>%
      mutate(response_code = case_when(
        trimws(tolower(RSRESP)) ==
          "progressive disease" ~ 1,
        trimws(tolower(RSRESP)) ==
          "stable disease" ~ 2,
        trimws(tolower(RSRESP)) ==
          "partial response" ~ 3,
        trimws(tolower(RSRESP)) ==
          "complete response" ~ 4,
        TRUE ~ 0
      )) %>%
      group_by(SUBJID) %>%
      filter(response_code == max(response_code)) %>%
      ungroup() %>%
      select(-SUBJID) %>%
      group_by(ATRT, B_ECOG, RSRESP) %>%
      summarize(n = n(), .groups = "drop") %>%
      pivot_wider(names_from = ATRT,
                  values_from = n)

    attributes(test$RSRESP)$label <- NULL


    d <-  get_response(full_data, "SUBJID", "ATRT",
                       "RSRESP", NULL, "B_ECOG")[[1]]
    expect_true(identical(test, d))
  }
)


test_that(
  "The get_response() returns the right plot when only the additional grouping
  column is provided in the correct format (character).",
  {
    data(full_data)
    p <-  get_response(full_data[1:1000, ], "SUBJID", "ATRT", "RSRESP",
                       NULL, "B_ECOG")[[2]]
    vdiffr::expect_doppelganger("first-1000-ecoggroup-outcome", p)
  }
)



test_that(
  "The get_response() returns the right table when both the confirmed
  response column and the additional grouping column is provided in the
  correct format (character).",
  {
    data(full_data)
    test <- full_data[which(full_data$RSCONFYN == "Y"),
                      c("SUBJID", "ATRT", "RSRESP", "B_ECOG")] %>%
      distinct() %>%
      filter(trimws(tolower(RSRESP)) %in% c("progressive disease",
                                            "stable disease",
                                            "partial response",
                                            "complete response")) %>%
      mutate(response_code = case_when(
        trimws(tolower(RSRESP)) ==
          "progressive disease" ~ 1,
        trimws(tolower(RSRESP)) ==
          "stable disease" ~ 2,
        trimws(tolower(RSRESP)) ==
          "partial response" ~ 3,
        trimws(tolower(RSRESP)) ==
          "complete response" ~ 4,
        TRUE ~ 0
      )) %>%
      group_by(SUBJID) %>%
      filter(response_code == max(response_code)) %>%
      ungroup() %>%
      select(-SUBJID) %>%
      group_by(ATRT, B_ECOG, RSRESP) %>%
      summarize(n = n(), .groups = "drop") %>%
      pivot_wider(names_from = ATRT,
                  values_from = n)

    attributes(test$RSRESP)$label <- NULL


    d <-  get_response(full_data, "SUBJID", "ATRT",
                       "RSRESP", "RSCONFYN", "B_ECOG")[[1]]
    expect_true(identical(test, d))
  }
)



test_that(
  "The get_response() returns the right plot when both the confirm response
  column and the additional grouping column is provided in the correct format
  (character).",
  {
    data(full_data)
    p <-  get_response(full_data[1:1000, ], "SUBJID", "ATRT", "RSRESP",
                       "RSCONFYN", "B_ECOG")[[2]]
    vdiffr::expect_doppelganger("first-1000-ecoggroup-confirmed-outcome", p)
  }
)
