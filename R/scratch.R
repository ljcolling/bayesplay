# extract_posterior_function <- function(x) {
#   x$posterior_function
# }
# 
# extract_prior_function <- function(x) {
#   x@prior_obj$prior_function
# }
# 
# diff <- function(x) {
#   po <- extract_posterior_function(x)
#   pr <- extract_prior_function(x)
#   function(arg) po(arg) / pr(arg)
# }
# 
# data1 <- likelihood("binomial", 7, 10)
# prior1 <- prior("beta", 2.5, 1)
# prior0 <- prior("point", 0.5)
# post1 <- data1 * prior1
# post0 <- data1 * prior0
# integral(post1) / integral(post0)
# fn <- diff(post1)
# 
# library(BayesFactor)
# set.seed(666)
# n1 = 145
# n2 = 100
# x = rnorm(n1, 44, 67)
# y = rnorm(n2, 32, 56)
# sqrt((n1 * n2)/ (n1 + n2))
# BayesFactor::ttestBF(x = x, y = y, paired = FALSE, rscale = .707)
# tobj <- t.test(x, y, paired = FALSE, var.equal = TRUE)
# t = as.numeric(tobj$statistic)
# df = as.numeric(tobj$parameter)
# data_model <- likelihood("noncentral_t", t, df)
# prior_model <- prior("cauchy", 0, 0.707 * sqrt((n1 * n2)/ (n1 + n2)))
# bayesplay::sd_ratio(data_model * prior_model, 0)
# 
