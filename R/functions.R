
`%||%` <- function(x, y) { # nolint
  if (purrr::is_empty(x)) y else x
}

in_range <- function(x, range) {

  min_value <- range[1]
  max_value <- range[2]

  if (all(c(x >= min_value, x <= max_value))) {
    return(TRUE)
  } else {
    return(FALSE)
  }


}


# function factory for distribution functions
make_distribution <- function(dist_name, params) {

  g <- glue::glue

  eval2 <- function(x) {
    eval(parse(text = x))
  }

# add x = theta if it's missing
  if ("x" %in% names(params) == FALSE) {
    params <- c(list(x = "theta"), params)
  }

# point
  point <- g("function({params$x})",
             "ifelse({params$x} == {params$point}",
             ", 1, 0)")

# normal distribution
  norm_dist <- g("function({params$x}) ",
                  "dnorm(x = {params$mean},",
                  " mean = {params$x},",
                  " sd = {params$sd})")

# half normal distribution
  half_norm <- g("function({params$x})",
                  " ifelse(in_range({params$x},",
                  "c({params$range[1]},{params$range[2]})),",
                  " dnorm(x = {params$mean},",
                  " mean = {params$x},",
                  " sd = {params$sd}) * {params$k}, 0)")


# uniform distribution
  uni_dist <- g("function({params$x})",
                 " dunif(x = {params$x},",
                 " min = {params$min},",
                 " max = {params$max})")


# t distribution
  t_dist <- g("function({params$x})",
               " dt_scaled(x = {params$mean},",
               " df = {params$df},",
               " mean = {params$x},",
               # " ncp = {params$ncp},",
               " sd = {params$sd})")

  non_central_t_dist <- g("function({params$x})",
                          " dt(x = {params$d} * sqrt(params$df + 1),",
                          " df = {params$df},",
                          " ncp = {sqrt(params$df + 1)} * {params$x})")

  non_central_t_dist_t <- g("function({params$x})",
                          " dt(x = {params$t},",
                          " df = {params$df},",
                          " ncp = {params$x})")


  half_t <- g("function({params$x})",
               " ifelse(in_range({params$x},",
               "c({params$range[1]},{params$range[2]})),",
               " dt_scaled(x = {params$mean},",
               " df = {params$df},",
               " mean = {params$x},",
               " ncp = {params$ncp},",
               " sd = {params$sd}) * {params$k}, 0)")


# cauchy distribution
  cauchy_dist <- g("function({params$x})",
               " dcauchy(x = {params$x},",
               " location = {params$location},",
               " scale = {params$scale})")

# half t-distribution

  half_cauchy <- g("function({params$x})",
               " ifelse(in_range({params$x},",
               "c({params$range[1]},{params$range[2]})),",
               " dcauchy(x = {params$x},",
               " location = {params$location},",
               " scale = {params$scale}) * {params$k}, 0)")


  return_func <- eval(parse(text = dist_name))
  return(eval2(return_func))
}



#' @importFrom methods new
#' @importFrom stats qnorm sd integrate


#' @S3Method
#' @export
`*.bayesplay` <- function(e1, e2) {
  if (class(e1) == "likelihood") {
    likelihood <- e1
    prior <- e2
  } else {
    likelihood <- e2
    prior <- e1
  }

  theta_range <- prior@theta_range

  likelihood_func <- likelihood@func
  prior_func <- prior@func

  # normalise the pior
  k <- 1 # can delete this

  posterior_mod <- calc_posterior(likelihood, prior)
  marginal <- function(theta) {
      likelihood_func(theta = theta) * (prior_func(theta = theta))
    }

    if (theta_range[1] != theta_range[2]) {
      alt_val <- suppressWarnings(stats::integrate(marginal,
                                                   theta_range[1],
                                                   theta_range[2])$value) # nolin
    } else {
      alt_val <- marginal(theta_range[[1]])
    }
  # alt_func <- marginal
  data <- list(
    integral = alt_val,
    # marginal = marginal,
    posterior_function = posterior_mod,
    prediction_function = predict(likelihood, prior),
    prior.normalising.constant = k
  )

  desc <- paste0(
  "Object of class predictive\n",
  "Likelihood family: ", likelihood$family, "\n",
  "Prior family: ", prior$family, "\n\n",
  "Area under curve: ", round(data$integral,4)
  )

    
  new(
    Class = "predictive",
    desc = desc,
    data = data,
    K = k,
    lik = likelihood_func,
    prior = prior_func,
    theta_range = theta_range,
    likelihood_obj = likelihood,
    prior_obj = prior
  )
}
# }

#' @S3Method
`/.predictive` <- function(e1, e2) {
  bf <-e1@data$integral / e2@data$integral
  return(bf)
}

setOldClass("numeric")

auc <- setClass("auc", contains = "numeric")
bf <- setClass("bf", contains = "numeric")



#' Compute integral
#'
#' Computes the definite integral of a \code{predictive} object over the range
#' of the parameter
#'
#' @param obj a \code{predictive} object
#'
#' @return A numeric of the point predictive value
#' @export
#'
#' @examples
integral <- function(obj) {
  new("auc", obj$integral)
}

#' @export
`/.auc` <- function(e1, e2) {
  new("bf", unclass(e1) / unclass(e2))
}

bfsay = function(BF) {
  BF <- unclass(BF)
  if(BF < 1){
    BF_base <- BF
    BF <- 1 / BF
  } else {
    BF_base <- BF
  }

  ev_level <- dplyr::case_when(BF == 1 ~ "No evidence",
                              BF > 1 & BF <= 3 ~ "Anecdotal evidence",
                              BF > 3 & BF <= 10 ~ "Moderate evidence",
                              BF > 10 & BF <= 30 ~ "Strong evidence",
                              BF > 30 & BF <= 100 ~ "Very strong evidence",
                              BF > 100 ~ "Extreme evidence")


 cat("Using the levels from  Wagenmakers et al (2017)\n")
 cat("A BF of ", round(BF_base,4), " indicates:\n")
 cat(ev_level)



}

#' Summary for an object of class \code{bf}
#' @noRd
#' @export
setMethod(
  "summary",
  "bf",
  function(object) {
    cat("Bayes factor\n")
    cat(bfsay(object), "\n")
    # cat(object, "\n")
  }
)

#' Summary for an object of class \code{bf}
#' @noRd
#' @export
setMethod(
  "show",
  "bf",
  function(object) {
    cat(object, "\n")
  }
)

#' Summary for an object of class \code{bf}
#' @noRd
#' @export
setMethod(
  "show",
  "auc",
  function(object) {
    cat(object, "\n")
  }
)

predict <- function(data_model, prior_model) {

  g <- glue::glue

  marginal <- data_model@marginal #nolint
  marginal_func <- eval(parse(text = g("function(x) {marginal}"))) #nolint

  predictive_func <- function(x) integral(marginal_func(x) * prior_model)

  return(Vectorize(predictive_func))

}

