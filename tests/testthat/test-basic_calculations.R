test_that("basic BF calculations", {
  tol <- 0.005
  data_model <- likelihood(distribution = "normal", mean = 5, sd = 10)
  h1_model <- prior(distribution = "uniform", 0, 20)
  h0_model <- prior(distribution = "point", point = 0)
  m1 <- integral(data_model * h1_model)
  m0 <- integral(data_model * h0_model)
  b <- m1 / m0

  testthat::expect_equal(b, 0.89, tolerance = tol, scale = 1,
                        label = "uniform prior")

  data_model <- likelihood(distribution = "normal", mean = 5.5, sd = 32.35)
  h0_model <- prior(distribution = "point", point = 0)
  h1_model <- prior(distribution = "normal", mean = 0, sd = 13.3,
                    range = c(0, Inf))
  m1 <- integral(data_model * h1_model)
  m0 <- integral(data_model * h0_model)
  b <- m1 / m0

  testthat::expect_equal(b, 0.97, tolerance = tol, scale = 1,
                         label = "normal prior")



  data_model <- likelihood(distribution = "normal", mean = 0.63, sd = 0.43)
  h1_model <- prior(distribution = "normal", mean = 0, sd = 2.69,
                    range = c(0, Inf))
  h0_model <- prior(distribution = "point", point = 0)
  m1 <- integral(data_model * h1_model)
  m0 <- integral(data_model * h0_model)
  b <- m1 / m0

  testthat::expect_equal(b, 0.83, tolerance = tol, scale = 1,
                         label = "half-normal prior")


  data_model <- likelihood(distribution = "normal", mean = 15, sd = 13)
  h1_model <- prior(distribution = "normal", mean = 50, sd = 14)
  h0_model <- prior(distribution = "point", point = 0)
  m1 <- integral(data_model * h1_model)
  m0 <- integral(data_model * h0_model)
  b <- m1 / m0

  testthat::expect_equal(b, 0.25, tolerance = tol, scale = 1,
                         label = "normal prior")


  data_model <- likelihood("student_t", mean = 5.47, sd = 32.2, df = 119)
  h1_model <- prior("student_t", mean = 13.3, sd = 4.93, df = 72)
  h0_model <- prior("point", 0)
  m1 <- integral(data_model * h1_model)
  m0 <- integral(data_model * h0_model)
  b <- m1 / m0

  testthat::expect_equal(b, 0.97, tolerance = tol, scale = 1,
                         label = "student_t prior (student_t likelihood)")

  t <- 2.03
  n <- 80
  d <- t / sqrt(n)
  data_model <- likelihood(distribution = "noncentral_d", d = d, df = n - 1)

  h1_model <- prior("cauchy", scale = 1)
  h0_model <- prior("point", 0)
  m1 <- integral(data_model * h1_model)
  m0 <- integral(data_model * h0_model)

  b1 <- m1 / m0
  b2 <-  1 / 1.557447

  testthat::expect_equal(b1, unname(b2),
                         label = "default bayes t (orginal)",
                         tolerance = tol, scale = 1
  )



  data_model <- likelihood(distribution = "noncentral_t",
                           t = t, df = n - 1)

  h1_model <- prior("cauchy", scale = 1 * sqrt(n))
  h0_model <- prior("point", 0)
  m1 <- integral(data_model * h1_model)
  m0 <- integral(data_model * h0_model)

  b1 <- m1 / m0

  testthat::expect_equal(b1, unname(b2),
                         label = "default bayes t (t version)",
                         tolerance = tol, scale = 1)

})
