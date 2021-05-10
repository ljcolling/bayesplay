#' @noRd
#' @export
prior <- setClass(
  Class = "prior",
  slots = list(
    data = "list",
    theta_range = "numeric",
    func = "function",
    desc = "character",
    type = "character",
    dist_type = "character",
    plot = "list",
    parameters = "list",
    function_text = "character"
  )
)


#' @noRd
#' @export
likelihood <- setClass(
  Class = "likelihood",
  slots = list(
    data = "list",
    # theta_range = "",
    func = "function",
    desc = "character",
    observation = "numeric",
    # type = "character",
    dist_type = "character",
    plot = "list",
    # parameters = "",
    marginal = "character"
  )
)



#' @noRd
#' @export
product <- setClass(
  Class = "product",
  slots = list(
    data = "list",
    desc = "character",
    K = "numeric",
    lik = "function",
    prior = "function",
    theta_range = "numeric",
    likelihood_obj = "likelihood",
    prior_obj = "prior"
  )
)


#' @noRd
#' @export
posterior <- setClass(
  Class = "posterior",
  slots = list(
    data = "list",
    desc = "character",
    K = "numeric",
    lik = "function",
    prior = "function",
    theta_range = "numeric",
    likelihood_obj = "likelihood",
    prior_obj = "prior"
  )
)

#' @noRd
#' @export
prediction <- setClass(
  Class = "prediction",
  slots = list(
    data = "list",
    desc = "character",
    K = "numeric",
    lik = "function",
    prior = "function",
    theta_range = "numeric",
    likelihood_obj = "likelihood",
    prior_obj = "prior"
  )
)


setClassUnion("bayesplay", c(
  "likelihood",
  "prior",
  "product",
  "posterior",
  "prediction"
))

#' Summary for an object of class \code{bayesplay}
#' @noRd
#' @export
setMethod(
  "show",
  "bayesplay",
  function(object) {
    cat(object@desc, "\n")
  }
)

#' Access values stored in the data slot of an object of class \code{bayesplay}
#' @usage \\method{$}{bayesplay}(object, ...)
#' @noRd
#' @export
setMethod("$",
  signature = "bayesplay",
  function(x, name) {
    returnval <- x@data[[name]]
    return(returnval)
  }
)


#' Get names of the data slot of an object of class \code{bayesplay}
#' @usage \\method{names}{bayesplay}(object, ...)
#' @noRd
#' @export
setMethod("names",
  signature = "bayesplay",
  function(x) {
    return(names(x@data))
  }
)

# constants
theta <- "\u03F4"
posterior_labs <- list(x = theta, y = "Density")
likelihood_labs <- list(x = theta, y = "Pr(Outcome)")



#' @export
setClass(
  Class = "normal",
  list(
    family = "character",
    fun = "function",
    default_range = "numeric"
  ),
  prototype = list(
    family = "normal",
    fun = function(x, mean, sd, ...) {
      dnorm(x = mean, mean = x, sd = sd)
    },
    default_range = c(-Inf, Inf)
  )
)


#' @export
setClass(
  Class = "point",
  list(
    family = "character",
    fun = "function",
    default_range = "numeric"
  ),
  prototype = list(
    family = "normal",
    fun = function(x, point, ...) {
      ifelse(x == point, 1, 0)
    },
    default_range = c(-Inf, Inf)
  )
)


#' @export
setClass(
  Class = "uniform",
  list(
    family = "character",
    fun = "function",
    default_range = "numeric"
  ),
  prototype = list(
    family = "normal",
    fun = function(x, min, max, ...) {
      dunif(x = x, min = min, max = max)
    },
    default_range = c(-Inf, Inf)
  )
)




#' @export
setClass(
  Class = "student_t",
  list(family = "character", fun = "function", default_range = "numeric"),
  prototype = list(
    family = "student_t",
    fun = function(x, mean, sd, df) {
      dt_scaled(x = mean, mean = x, sd = sd, df = df)
    },
    default_range = c(-Inf, Inf)
  )
)


#' @export
setClass(
  Class = "cauchy",
  list(family = "character", fun = "function", default_range = "numeric"),
  prototype = list(
    family = "cauchy",
    fun = function(x, location = 0, scale) {
      dcauchy(x = x, location = location, scale = scale)
    },
    default_range = c(-Inf, Inf)
  )
)

#' @export
setClass(
  Class = "beta",
  list(family = "character", fun = "function", default_range = "numeric"),
  prototype = list(
    family = "cauchy",
    fun = function(x, alpha, beta) {
      dbeta(x = x, shape1 = alpha, shape2 = beta)
    },
    default_range = c(0, 1)
  )
)

#' @export
setClass(
  Class = "noncentral_t",
  list(family = "character", fun = "function"),
  prototype = list(
    family = "noncentral_t",
    fun = function(x, t, df) {
      dt(x = t, df = df, ncp = x)
    }
  )
)


#' @export
setClass(
  Class = "noncentral_d",
  list(family = "character", fun = "function"),
  prototype = list(
    family = "noncentral_d",
    fun = function(x, d, n) {
      dt(x = d * sqrt(n), df = n - 1, ncp = x * sqrt(n))
    }
  )
)

setClass(
  Class = "noncentral_d2",
  list(family = "character", fun = "function"),
  prototype = list(
    family = "noncentral_d2",
    fun = function(x, d, n1, n2) {
      dt(
        x = d / sqrt(1 / n1 + 1 / n2),
        df = n1 + n2 - 2,
        ncp = x * sqrt((n1 * n2) / (n1 + n2))
      )
    }
  )
)

setClass(
  Class = "binomial",
  list(family = "character", fun = "function"),
  prototype = list(
    family = "binomial",
    fun = function(p, successes, trials) {
      dbinom(prob = p, size = trials, x = successes)
    }
  )
)



setClassUnion(
  "family",
  c(
    "normal",
    "student_t",
    "noncentral_t",
    "noncentral_d",
    "noncentral_d2",
    "binomial",
    "point",
    "uniform",
    "cauchy"
  )
)
