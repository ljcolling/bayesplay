context("Error messages")
test_that("error messages", {


  # missing parameters

  expect_error(
    prior("uniform", 0),
    "You must specify `min` and `max` for a uniform  prior"
  )

  expect_error(
    prior("normal", 0),
    "You must specify `mean` and `sd` for a normal prior"
  )


  expect_warning(
    prior("point"),
    "Point value is missing. Assuming 0"
  )

  expect_error(
    prior("student_t"),
    "You must specify `mean`, `sd`, and `df` for a student_t prior"
  )


  expect_error(
    prior("beta"),
    "You must specify `alpha` and `beta` for a beta  prior"
  )


  #   expect_error(prior("cauchy"),
  # "You must specify `location` and `scale` for a cauchy  prior")
  data <- likelihood(distribution = "binomial", 2, 10)
  m0 <- data * prior(distribution = "beta", 10, 10)
  m1 <- data *  prior("point", 0.5)
  expect_error(
               visual_compare(m0, m1, type = "null"),
        "Type null not supported")

})
