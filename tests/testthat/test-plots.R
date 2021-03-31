context("Plots")
test_that("Prior plots", {

  # Half normal prior
  half_normal <- prior("normal", 0, 45, range = c(0, Inf))
  half_normal_plot <- plot(half_normal)
  vdiffr::expect_doppelganger("half normal prior", half_normal_plot)

  normal <- prior("normal", 0, 45)
  normal_plot <- plot(normal)
  vdiffr::expect_doppelganger("normal prior", normal_plot)



  uniform <- prior("uniform", -10, 10)
  uniform_plot <- plot(uniform)
  vdiffr::expect_doppelganger("uniform prior", uniform_plot)

  point <- prior("point", 2)
  point_plot <- plot(point)
  vdiffr::expect_doppelganger("point prior", point_plot)


  # likelihoods
  vdiffr::expect_doppelganger("binomial likelihood", plot(likelihood("binomial", 2, 10)))
  vdiffr::expect_doppelganger("normal likelihood", plot(likelihood("normal", 0, 1)))



  # visual comparison discrete
  prior_model <- prior(distribution = "beta", 10, 10)
  data_model <- likelihood(distribution = "binomial", 2, 10)
  null_prior <- prior("point", 0.5)

  m0 <- data_model * null_prior
  m1 <- data_model * prior_model
  comparison_plot1 <- visual_compare(m0, m1)
  vdiffr::expect_doppelganger("discrete visual compare", comparison_plot1)


  # visual comparison continuous
  prior_model <- prior(distribution = "normal", 0, 13.13)
  data_model <- likelihood(distribution = "normal", 5.5, 32.35)
  null_prior <- prior("point", 0)
  m0 <- data_model * null_prior
  m1 <- data_model * prior_model
  comparison_plot2 <- visual_compare(m0, m1)
  vdiffr::expect_doppelganger("continuous visual compare", comparison_plot2)


  # ratio plot continuous
  prior_model <- prior(distribution = "normal", 0, 10)
  data_model <- likelihood(distribution = "normal", 5, 5)
  null_prior <- prior("point", 0)
  m0 <- data_model * null_prior
  m1 <- data_model * prior_model
  ratio_plot1 <- visual_compare(m0, m1, type = "ratio")
  vdiffr::expect_doppelganger("continuous ratio plot", ratio_plot1)

  # ratio plot discrete

  prior_model <- prior(distribution = "beta", 10, 10)
  data_model <- likelihood(distribution = "binomial", 2, 10)
  null_prior <- prior("point", 0.5)

  m0 <- data_model * null_prior
  m1 <- data_model * prior_model
  ratio_plot2 <- visual_compare(m0, m1, type = "ratio")
  vdiffr::expect_doppelganger("discrete ratio plot", ratio_plot2)

  # beta-binom
  prior_model <- prior(distribution = "beta", 10, 10)
  data_model <- likelihood(distribution = "binomial", 2, 10)
  m1 <- data_model * prior_model
  prior_posterior1 <-  plot(m1, type = "pp")
  posterior1 <- plot(m1, type = "posterior")
  lik <- plot(m1)
  vdiffr::expect_doppelganger("beta-binom pp", prior_posterior1)
  vdiffr::expect_doppelganger("beta-binom posterior", posterior1)
  vdiffr::expect_doppelganger("beta-binom weighted likelihood", lik)
  

  # normal-normal
  prior_model <- prior(distribution = "normal", 0, 10)
  data_model <- likelihood(distribution = "normal", 5, 5)
  m1 <- data_model * prior_model
  prior_posterior2  <-  plot(m1, type="pp")
  posterior1 <- plot(m1, type = "posterior")
  lik <- plot(m1)
  vdiffr::expect_doppelganger("normal-normal pp", prior_posterior2)
  vdiffr::expect_doppelganger("normal posterior", posterior1)
  vdiffr::expect_doppelganger("normal weighted likelihood", lik)

})
