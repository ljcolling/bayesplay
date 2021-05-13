context("Likelihoods")
test_that("specifying likelihoods", {
  tol <- 0.0000005
  half_norm <- prior(
    family = "normal",
    mean = 0,
    sd = 10,
    range = c(0, Inf)
  )
})
