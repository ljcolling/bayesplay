
get_function <- function(x) x@fun
get_family <- function(x) x@family

make_likelihood_data <- function(family, params, func) {
  list(
    family = get_family(family),
    parameters = as.data.frame(params),
    likelihood_function = func
  )
}

likelihood_data_names <- c("family", "parameters", "likelihood_function")

dt_scaled <- function(x, df, mean = 0, sd = 1, ncp = 0, log = FALSE) {
  # if (ncp == 0) {
  if (!log) {
    return(stats::dt((x - mean) / sd, df, ncp = ncp, log = FALSE) / sd)
  } else {
    return(stats::dt((x - mean) / sd, df, ncp = ncp, log = TRUE) - log(sd))
  }
}

#' @export
setClass(
  Class = "normal",
  list(family = "character", fun = "function"),
  prototype = list(
    family = "normal",
    fun = function(x, mean, sd) {
      dnorm(x = mean, mean = x, sd = sd)
    }
  )
)


#' @export
setClass(
  Class = "student_t",
  list(family = "character", fun = "function"),
  prototype = list(
    family = "student_t",
    fun = function(x, mean, sd, df) {
      dt_scaled(x = mean, mean = x, sd = sd, df = df)
    }
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
  c("normal",
    "student_t",
    "noncentral_t",
    "noncentral_d",
    "noncentral_d2",
    "binomial")
)


d_variance <- function(d, df) {
  (df + df + 2) / ((df + 1) * (df + 1)) + ((d * d) / (2 * (df + df + 2)))
}


d2_variance <- function(d, n1, n2) {
  (n1 + n2) / ((n1) * (n2)) + ((d * d) / (2 * (n1 + n2)))
}

likelihood_data_names <- c("family", "parameters", "likelihood_function")
#################################################################
##                 DEFINITIONS OF THE LIKELIHOODS              ##
#################################################################


#' Specify a likelihood
#' @description Define likelihoods using different different distribution families
#' @param family the likelihood distribution (see details)
#' @param ... see details
#'
#' @details
#' ## Available distribution families
#' The following distribution families can be used for the likelihood
#' * \code{normal} a normal distribution
#' * \code{student_t} a scaled and shifted t-distribution
#' * \code{noncentral_t} a noncentral t (for t statistic)
#' * \code{noncentral_d} a noncentral t (for one sample d)
#' * \code{noncentral_d2} a noncentral t (for independent samples d)
#' The parameters that need to be specified will be dependent on the
#' family
#' ## normal distribution
#' When \code{family} is set to \code{normal} then the following
#' parameters must be set
#' * \code{mean} mean of the normal likelihood
#' * \code{sd} standard deviation of the normal likelihood
#'
#' ## student_t distribution
#' When \code{family} is set to \code{student_t} then the following
#' parameters may be set
#' * \code{mean} mean of the scaled and shifted t likelihood
#' * \code{sd} standard deviation of the scaled and shifted t likelihood
#' * \code{df} degrees of freedom
#'
#' ## noncentral_t distribution
#' When \code{family} is set to \code{noncentral_t} then the following
#' parameters may be set
#' * \code{t} the t value of the data
#' * \code{df} degrees of freedom
#'
#' ## noncentral_d distribution
#' When \code{family} is set to \code{noncentral_d} then the following
#' parameters may be set
#' * \code{d} the d (mean / sd) value of the data
#' * \code{n} the sample size
#'
#' ## noncentral_d2 distribution
#' When \code{family} is set to \code{noncentral_d2} then the following
#' parameters may be set
#' * \code{d} the d (mean / s_pooled) value of the data
#' * \code{n1} the sample size of group 1
#' * \code{n2} the sample size of group 2
#'
#' \eqn{s_{\mathrm{pooled}}}{s_pooled} is set as below:
#' \deqn{s_{\mathrm{pooled}} = \sqrt{\frac{(n_1 - 1)s^2_1 + (n_2 - 1)s^2_2 }{n_1 + n_2 - 2}}}{\sqrt(((n1 - 1) * s1^2 + (n2 - 1)*s2^2)/(n1 + n2 - 2))}
#'
#'
#' @md
#' @return an object of class \code{likelihood}
#' @export
#'
#' @examples
#' # specify a normal likelihood
#' likelihood(family = "normal", mean = 5.5, sd = 32.35)
#'
#' # specify a scaled and shifted t likelihood
#' likelihood(family = "student_t", mean = 5.5, sd = 32.35, df = 10)
#'
#' # specify non-central t likelihood (t scaled)
#' likelihood(family = "noncentral_t", t = 10, df = 10)
#'
#' # specify non-central t likelihood (d scaled)
#' likelihood(family = "noncentral_d", d = 10, n = 10)
#'
#' # specify non-central t likelihood (independent samples d scaled)
#' likelihood(family = "noncentral_d2", d = 10, n1 = 10, n2 = 12)
likelihood <- function(family, ...) {
  if (!methods::existsMethod(signature = family, f = "make_likelihood")) {
    stop(family, " is not a valid distribution family")
  }
  make_likelihood(family = new(family), ...)
}

likelihood_labs <- list(x = "theta", y = "Pr(Outcome)")



setGeneric("make_likelihood",
  signature = "family",
  function(family, ...) UseMethod("make_likelihood")
)




setMethod(
  "make_likelihood",
  signature(family = "normal"),
  function(family, mean, sd) {
    make_likelihood.normal(family, mean, sd)
  }
)


setMethod(
  "make_likelihood",
  signature(family = "student_t"),
  function(family, mean, sd, df) {
    make_likelihood.student_t(family, mean, sd, df)
  }
)

setMethod(
  "make_likelihood",
  signature(family = "noncentral_t"),
  function(family, t, df) {
    make_likelihood.noncentral_t(family, t, df)
  }
)


setMethod(
  "make_likelihood",
  signature(family = "noncentral_d"),
  function(family, d, n) {
    make_likelihood.noncentral_d(family, d, n)
  }
)

setMethod(
  "make_likelihood",
  signature(family = "noncentral_d2"),
  function(family, d, n1, n2) {
    make_likelihood.noncentral_d2(family, d, n1, n2)
  }
)

setMethod(
  "make_likelihood",
  signature(family = "binomial"),
  function(family, successes, trials) {
    make_likelihood.binomial(family, successes, trials)
  }
)



#' @method likelihood normal
#' @usage likelihood(family = "normal", mean, sd)
#' @rdname likelihood
make_likelihood.normal <- function(family, mean, sd) { # nolint
  if (missing(mean) | missing(sd)) {
    stop("You must specify a `mean` and `sd` for a normal likelihood",
      call. = FALSE
    )
  }

  if (sd <= 0) {
    stop("`sd` must be greater than 0")
  }

  params <- list(mean = mean, sd = sd)

  # calculate the plot defaults
  width <- 4 * sd
  range <- c(mean - width, mean + width)

  # func <- make_distribution(
  #   "norm_dist",
  #   list(mean = mean, sd = sd)
  # )


  func <- function(x) get_function(family)(x = x, mean = mean, sd = sd)



  data <- make_likelihood_data(family, params, func)

  desc <- describe_likelihood(family = data$family, params = params)
  new(
    Class = "likelihood",
    func = func,
    data = data,
    marginal = paste0(
      "likelihood(family = \"normal\", mean = x, sd = ",
      sd, ")"
    ),
    observation = params$mean,
    desc = desc,
    dist_type = "continuous",
    plot = list(
      range = range,
      labs = likelihood_labs
    )
  )
}


#' @method likelihood student_t
#' @usage likelihood(family = "student_t", mean, sd, df)
#' @rdname likelihood
make_likelihood.student_t <- function(family, mean, sd, df) {
  if (df == 0) {
    stop("You must specify a `df` a student_t likelihood",
      call. = FALSE
    )
  }

  if (sd <= 0) {
    stop("`sd` must be greater than 0")
  }


  if (df <= 0) {
    stop("`df` muist be greater than 0")
  }

  params <- list(mean = mean, sd = sd, df = df)


  # calculate the plot defaults
  width <- 4 * sd
  range <- c(mean - width, mean + width)
  #   func <- make_distribution(
  #     "t_dist",
  #     list(df = df, mean = mean, sd = sd)
  #   )
  #

  func <- function(x) get_function(family)(x = x, mean = mean, sd = sd, df = df)

  data <- make_likelihood_data(family = family, params = params, func = func)
  desc <- describe_likelihood(family = data$family, params = params)

  new(
    Class = "likelihood",
    data = data,
    func = func,
    marginal = paste0(
      "likelihood(family = \"student_t\", mean = x, sd = ",
      sd, ", df = ", df, ")"
    ),
    observation = params$mean,
    desc = desc,
    dist_type = "continuous",
    plot = list(
      range = range,
      labs = likelihood_labs
    )
  )
}


#' @method likelihood noncentral_d
#' @usage likelihood(family = "noncentral_d", d, n)
#' @rdname likelihood
make_likelihood.noncentral_d <- function(family, d, n) {
  if (n == 0) {
    stop("You must specify a `n` a non-central likelihood",
      call. = FALSE
    )
  }

  params <- list(d = d, n = n)

  # calculate the plot defaults
  variance <- d_variance(d, n - 1)
  sd <- sqrt(variance)
  min <- d - 4 * sd
  max <- d + 4 * sd
  range <- c(min, max)

  func <- function(x) get_function(family)(x = x, d = d, n = n)
  data <- make_likelihood_data(family = family, params = params, func = func)
  desc <- describe_likelihood(family = data$family, params = params)

  new(
    Class = "likelihood",
    data = data,
    func = func,
    marginal = paste0(
      "likelihood(family = \"noncentral_d\", d = x, n = ",
      n, ")"
    ),
    observation = params$d,
    desc = desc, dist_type = "continuous",
    plot = list(
      range = range,
      labs = likelihood_labs
    )
  )
}

#' @method likelihood noncentral_t
#' @usage likelihood(family = "noncentral_t", t, df)
#' @rdname likelihood
make_likelihood.noncentral_t <- function(family, t, df) {
  params <- list(t = t, df = df)

  d <- t * sqrt(df + 1)
  variance <- d_variance(d, df)
  sd <- sqrt(variance)
  min <- t - 4 * sd
  max <- t + 4 * sd
  # calculate the plot defaults
  range <- c(min, max)
  func <- function(x) get_function(family)(x = x, t = t, df = df)
  data <- make_likelihood_data(family = family, params = params, func = func)

  desc <- describe_likelihood(family = data$family, params = params)
  new(
    Class = "likelihood",
    data = data,
    func = func,
    marginal = paste0(
      "likelihood(family = \"noncentral_t\", t = x, df = ",
      df, ")"
    ),
    observation = params$t,
    desc = desc,
    dist_type = "continuous",
    plot = list(
      range = range,
      labs = likelihood_labs
    )
  )
}


#' @method likelihood binomial
# #' @usage likelihood(family = "binomial", successes, trials)
#' @rdname likelihood
make_likelihood.binomial <- function(family, successes, trials) {
  params <- list(successes = successes, trials = trials)
  # calculate the plot defaults
  range <- c(0, 1)

  func <- function(p) get_function(family)(p = p, successes = successes, trials = trials)

  data <- make_likelihood_data(family = family, params = params, func = func)
  desc <- describe_likelihood(family = data$family, params = params)

  new(
    Class = "likelihood",
    func = func,
    data = data,
    marginal = paste0(
      "likelihood(family = \"binomial\", successes = ",
      successes, ", trials =  trials)"
    ),
    observation = params$successes,
    desc = desc,
    # dist_type = "discrete",
    dist_type = "continuous",
    plot = list(
      range = range,
      labs = likelihood_labs
    )
  )
}

#' @method likelihood noncentral_d2
#' @usage likelihood(family = "noncentral_d2", d, n1, n2)
#' @rdname likelihood
make_likelihood.noncentral_d2 <- function(family, d, n1, n2) {
  if (n1 == 0 | n2 == 0) {
    stop("You must specify a `n1` and `n2` a non-central t likelihood",
      call. = FALSE
    )
  }

  params <- list(d = d, n1 = n1, n2 = n2)

  # calculate the plot defaults
  variance <- d2_variance(d, n1, n2)
  sd <- sqrt(variance)
  min <- d - 4 * sd
  max <- d + 4 * sd
  range <- c(min, max)
  func <- function(x) get_function(family)(x = x, d = d, n1 = n1, n2 = n2)
  data <- make_likelihood_data(family = family, params = params, func = func)

  desc <- describe_likelihood(family = data$family, params = params)
  names(data) <- likelihood_data_names
  new(
    Class = "likelihood",
    data = data,
    func = func,
    marginal = paste0(
      "likelihood(family = \"noncentral_d2\", d = x,  n1 = ",
      n1, ", n2 = ", n2, ")"
    ),
    observation = params$d,
    desc = desc, dist_type = "continuous",
    plot = list(
      range = range,
      labs = likelihood_labs
    )
  )
}}
