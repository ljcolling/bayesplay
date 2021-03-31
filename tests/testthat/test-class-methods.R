test_that("multiplication works", {
  # expect_equal(2 * 2, 4)

  

  lik <- likelihood("normal", 0, 1)
  expect_equal(names(lik), c("family", "parameters", "fun"))


  pri <- prior("normal", 0, 1)
  expect_equal(names(pri), c("family", "parameters", "fun"))

  pred <- lik * pri
  expect_equal(names(pred),
  c("integral", "marginal", "posterior_function", "prediction_function",
  "prior.normalising.constant"))

  # check show methods
  # normal distribution
  lik1 <- likelihood("normal", 0, 1)
  expect_output(show(lik1), "Object of class likelihood\\nDistribution family: normal\\n\\nParameters\\nMean: 0\\nSD: 1 ")

  lik2 <- likelihood("normal", 2, 2)
  expect_output(show(lik2), "Object of class likelihood\\nDistribution family: normal\\n\\nParameters\\nMean: 2\\nSD: 2 ")


  pri1 <- prior("normal", 0, 1)
  expect_output(show(pri), "Object of class prior\\nDistribution family: normal\\n\\nParameters\nMean: 0\\nSD: 1\\nRange: -Inf to Inf ")


  expect_output(show(pred), "Object of class predictive\\nLikelihood family: normal\\nPrior family: normal\\n\\nArea under curve: 0\\.2821 ")


  pred1 <- lik * pri
  pred2 <- pri * lik
  expect_equal(pred1, pred2)

  expect_equal(integral(lik), 1)

})
