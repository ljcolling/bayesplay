
`%||%` <- function(x, y) { # nolint
  if (purrr::is_empty(x)) y else x
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
    alt_val <- stats::integrate(marginal, theta_range[1], theta_range[2])$value
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




#' Access values stored in the data slot of an object of class \code{predictive}
#' @usage \\method{$}{lilikelihood}(object, ...)
#' @noRd
#' @export
setMethod("$",
  signature = "predictive",
  function(x, name) {
    returnval <- x@data[[name]]
    return(returnval)
  }
)

#' Access values stored in the data slot of an object of class \code{likelihood}
#' @usage \\method{$}{lilikelihood}(object, ...)
#' @noRd
#' @export
setMethod("$",
  signature = "likelihood",
  function(x, name) {
    returnval <- x@data[[name]]
    return(returnval)
  }
)

#' Access values stored in the data slot of an object of class \code{prior}
#' @usage \\method{$}{lilikelihood}(object, ...)
#' @noRd
#' @export
setMethod("$",
          signature = "prior",
          function(x, name) {
            returnval <- x@data[[name]]
            return(returnval)
          }
)

#' Get names of the data slot of an object of class \code{likelihood}
#' @usage \\method{names}{likelihood}(object, ...)
#' @noRd
#' @export
setMethod("names",
  signature = "likelihood",
  function(x) {
    return(names(x@data))
  }
)

#' Get names of the data slot of an object of class \code{prior}
#' @usage \\method{names}{prior}(object, ...)
#' @noRd
#' @export
setMethod("names",
          signature = "prior",
          function(x) {
            return(names(x@data))
          }
)



#' Summary for an object of class \code{likelihood}
#' @noRd
#' @export
setMethod(
  "show",
  "likelihood",
  function(object) {
    cat("Object of class", class(object), "\n")
    cat("Likelihood type:", object@data$likelihood_type, "\n")
    cat(object@desc,"\n")
  }
)


#' Summary for an object of class \code{prior}
#' @noRd
#' @export
setMethod(
  "show",
  "prior",
  function(object) {
    cat("Object of class", class(object), "\n")
    cat("Prior type:", object@data$distribution, "\n")
    cat(paste0(object@desc,collapse = ""))
  }
)



#' Get names of the data slot of an object of class \code{predictive}
#' @usage \\method{names}{predictive}(object, ...)
#' @noRd
#' @export
setMethod("names",
  signature = "predictive",
  function(x) {
    return(names(x@data))
  }
)

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
