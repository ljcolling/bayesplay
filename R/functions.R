
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
  if (theta_range[1] != theta_range[2]) {
    k <- 1 / stats::integrate(
      f = prior_func,
      lower = theta_range[1],
      upper = theta_range[2]
    )$value
  } else {
    # It's a point prior
    k <- 1
  }
  marginal <- function(theta) {
    likelihood_func(theta = theta) * (k * prior_func(theta = theta))
  }

  if (theta_range[1] != theta_range[2]) {
    alt_val <- stats::integrate(marginal, theta_range[1], theta_range[2])$value # nolint
  } else {
    alt_val <- marginal(theta_range[[1]])
  }

  alt_func <- marginal
  data <- list(
    integral = alt_val,
    marginal = marginal,
    prior.normalising.constant = k
  )

  new(
    Class = "predictive",
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
  e1@data$integral / e2@data$integral
}



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
  obj$integral
}

