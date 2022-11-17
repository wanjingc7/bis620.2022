test_that(
  "The spectral_signature() errors when no X,Y,Z or time column",
  {
    data(iris)
    expect_error(accel_plot(iris))
  }
)


test_that(
  "The spectral_signature() returns a data.frame object.",
  {
    data(ukb_accel)
    d <-  spectral_signature(ukb_accel[1:100, ])
    expect_true(inherits(d, "data.frame"))
  }
)

test_that(
  "The spectral_signature() returns X,Y,Z and freq columns",
  {
    data(ukb_accel)
    d <-  spectral_signature(ukb_accel[1:100, ])
    expect_cols <- c("X", "Y", "Z", "freq")
    return_cols <- colnames(d)
    expect_true(identical(expect_cols, return_cols))
  }
)


test_that(
  "The spectral_signature() conducts correct fast discrete fourier
  transformation when taking inverse",
  {
    data(ukb_accel)

    d <- as.data.frame(ukb_accel[1:100, ]|>
      spectral_signature(inverse = TRUE)|>
      select(X, Y, Z))
    rownames(d) <- NULL


    test <- ukb_accel[1:100, ]|>
      select(X, Y, Z)


    expect_df <- c()
    x <- fft(unlist(test[, "X"]), inverse = TRUE)|>Mod()
    y <- fft(unlist(test[, "Y"]), inverse = TRUE)|>Mod()
    z <- fft(unlist(test[, "Z"]), inverse = TRUE)|>Mod()

    expect_df <- as.data.frame(cbind(x, y, z))
    expect_df <-  expect_df[seq_len(ceiling(nrow(expect_df) / 2)), ]
    colnames(expect_df) <- c("X", "Y", "Z")
    rownames(expect_df) <- NULL

    expect_true(identical(expect_df, d))
  }
)



test_that(
  "The spectral_signature() conducts correct fast discrete fourier
  transformation when not taking inverse",
  {
    data(ukb_accel)

    d <- as.data.frame(ukb_accel[1:100, ]|>
                         spectral_signature(inverse = FALSE)|>
                         select(X, Y, Z))
    rownames(d) <- NULL


    test <- ukb_accel[1:100, ]|>
      select(X, Y, Z)


    expect_df <- c()
    x <- fft(unlist(test[, "X"]), inverse = FALSE)|>Mod()
    y <- fft(unlist(test[, "Y"]), inverse = FALSE)|>Mod()
    z <- fft(unlist(test[, "Z"]), inverse = FALSE)|>Mod()

    expect_df <- as.data.frame(cbind(x, y, z))
    expect_df <-  expect_df[seq_len(ceiling(nrow(expect_df) / 2)), ]
    colnames(expect_df) <- c("X", "Y", "Z")
    rownames(expect_df) <- NULL

    expect_true(identical(expect_df, d))
  }
)


test_that(
  "The spectral_signature() conducts correct log transformation",
  {
    data(ukb_accel)
    d <-  ukb_accel[1:100, ] |>
      spectral_signature(take_log = FALSE) |>
      select(X, Y, Z)|>
      log()

    test <- ukb_accel[1:100, ]|>
      spectral_signature(take_log = TRUE)|>
      select(X, Y, Z)



    expect_true(identical(test, d))
  }
)



test_that(
  "The spectral_signature() conducts correct log transformation",
  {
    data(ukb_accel)
    d <-  ukb_accel[1:100, ] |>
      spectral_signature(take_log = FALSE) |>
     dplyr:: select(X, Y, Z)|>
      log()

    test <- ukb_accel[1:100, ]|>
      spectral_signature(take_log = TRUE)|>
      select(X, Y, Z)



    expect_true(identical(test, d))
  }
)

test_that(
  "The spectral_signature() conducts correct frequency calculation",
  {
    data(ukb_accel)
    d <-  ukb_accel[1:100, ] |>
      spectral_signature() |>
      select(freq)|>
      as.data.frame()

    test <- ukb_accel[1:100, ]


    longest_period <- as.numeric(difftime(max(test$time), min(test$time),
                                          units = "secs"))
    xt <- test$time[1:2]
    shortest_period <- as.numeric(difftime(max(xt), min(xt), units = "secs"))
    freq <- 1 / seq(longest_period, shortest_period, length.out = 50)

    expected <- as.data.frame(freq)




    expect_true(identical(expected, d))
  }
)
