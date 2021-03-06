test_that("class methods", {
  lik <- likelihood("normal", 0, 1)
  expect_equal(names(lik), c("family", "parameters", "likelihood_function"))
  pri <- prior("normal", 0, 1)
  expect_equal(names(pri), c("family", "parameters", "prior_function"))

  pred <- lik * pri
  expect_equal(
    names(pred),
    c(
      "integral",
      "marginal_function",
      "evidence_function",
      "posterior_function",
      "conditional_function",
      "weighted_likelihood_function",
      "prediction_function"
    )
  )

  # check show methods
  # normal distribution
  lik <- likelihood("normal", 0, 1)
  pri <- prior("normal", 0, 1)
  pred1 <- lik * pri
  pred2 <- pri * lik

  expect_equal(pred1, pred2)

  pr1 <- prior("cauchy", 0, 1, c(-Inf, Inf))
  pr2 <- make_prior(new("cauchy"), 0, 1, c(-Inf, Inf))
  expect_equal(pr1, pr2)
})
