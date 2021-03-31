
dt_scaled <- function(x, df, mean = 0, sd = 1, ncp = 0, log = FALSE) {
  # if (ncp == 0) {
  if (!log) {
    return(stats::dt((x - mean) / sd, df, ncp = ncp, log = FALSE) / sd)
  } else {
    return(stats::dt((x - mean) / sd, df, ncp = ncp, log = TRUE) - log(sd))
  }
}

d_variance <- function(d, df) {
  (df + df + 2) / ((df + 1) * (df + 1)) + ((d * d) / (2 * (df + df + 2)))
}

#################################################################
##                 DEFINITIONS OF THE LIKELIHOODS              ##
#################################################################


#' Specify a likelihood
#'
#' @param distribution the likelihood distribution (see details)
#' @param ... see details
#'
#' @details
#' ## Available distributions
#' The following distributions can be used for the likelihood
#' * \code{normal} a normal distribution
#' * \code{student_t} a scaled and shifted t-distribution
#' * \code{noncentral_t} a noncentral t distribution (t scaled)
#' * \code{noncentral_d} a noncentral t distribution (d scaled)
#' The parameters that need to be specified will be dependent on the
#' distribution
#' ## normal distribution
#' When \code{distribution} is set to \code{normal} then the following
#' parameters must be set
#' * \code{mean} mean of the normal likelihood
#' * \code{sd} standard deviation of the normal likelihood
#'
#' ## student_t distribution
#' When \code{distribution} is set to \code{student_t} then the following
#' parameters may be set
#' * \code{mean} mean of the scaled and shifted t likelihood
#' * \code{sd} standard deviation of the scaled and shifted t likelihood
#' * \code{df} degrees of freedom
#'
#' ## noncentral_t distribution
#' When \code{distribution} is set to \code{noncentral_t} then the following
#' parameters may be set
#' * \code{t} the t value of the data
#' * \code{df} degrees of freedom
#'
#' ## noncentral_d distribution
#' When \code{distribution} is set to \code{noncentral_d} then the following
#' parameters may be set
#' * \code{d} the d (mean / sd) value of the data
#' * \code{df} degrees of freedom
#'
#' @md
#' @return an object of class \code{likelihood}
#' @export
#'
#' @examples
#' # specify a normal likelihood
#' likelihood(distribution = "normal", mean = 5.5, sd = 32.35)
#'
#' # specify a scaled and shifted t likelihood
#' likelihood(distribution = "student_t", mean = 5.5, sd = 32.35, df = 10)
#'
#' # specify non-central t likelihood (t scaled)
#' likelihood(distribution = "noncentral_t", t = 10, df = 10)
#'
#' # specify non-central t likelihood (d scaled)
#' likelihood(distribution = "noncentral_d", d = 10, df = 10)
likelihood <- function(distribution, ...) {
  parameters <- as.list(match.call(expand.dots = TRUE))

  distribution <- paste0(parameters$distribution %||%
    "normal", "_likelihood")

  if (distribution %in% c(
    "normal_likelihood",
    "student_t_likelihood",
    "binomial_likelihood",
    "noncentral_d_likelihood",
    "noncentral_t_likelihood"
  ) == FALSE) {
    stop("Unkown likelihood function")
  }

  lik_fun <- purrr::partial(.f = rlang::as_function(distribution), ...)

  return(lik_fun())
}


# function that specifies a normal likelihood
normal_likelihood <- function(mean, sd) { # nolint
  if (missing(mean) | missing(sd)) {
    stop("You must specify a `mean` and `sd` for a normal likelihood",
      call. = FALSE
    )
  }


  params <- list(mean = mean, sd = sd)

  desc <- paste0(
    "Object of class likelihood\n",
    "Distribution family: normal\n\n",
    "Parameters\n",
    "Mean: ", params$mean, "\n",
    "SD: ", params$sd
  )


  # calculate the plot defaults
  width <- 4 * sd
  range <- c(mean - width, mean + width)

  func <- make_distribution(
    "norm_dist",
    list(mean = mean, sd = sd)
  )

  new(
    Class = "likelihood",
    data = list(
      family = "normal",
      parameters = as.data.frame(params),
      fun = func
    ),
    func = func,
    marginal = paste0(
      "likelihood(distribution = \"normal\", mean = x, sd = ",
      sd, ")"
    ),
    observation = params$mean,
    desc = desc,
    dist_type = "continuous",
    plot = list(
      range = range,
      labs = list(x = "\u03F4", y = "L(\u03F4|x)")
    )
  )
}


# function that specifies a student_t likelihood
student_t_likelihood <- function(mean = 0, sd = 1, df = 0) {
  if (df == 0) {
    stop("You must specify a `df` a student_t likelihood",
      call. = FALSE
    )
  }

  params <- list(mean = mean, sd = sd, df = df)
  desc <- paste0(
    "Object of class likelihood\n",
    "Distribution family: student t\n\n",
    "Parameters\n",
    "Mean: ", params$mean, "\n",
    "SD: ", params$sd, "\n",
    "DF:", params$df
  )


  # calculate the plot defaults
  width <- 4 * sd
  range <- c(mean - width, mean + width)
  func <- make_distribution(
    "t_dist",
    list(df = df, mean = mean, sd = sd)
  )


  new(
    Class = "likelihood",
    data = list(
      family = "student_t",
      parameters = as.data.frame(params),
      fun = func
    ),
    func = func,
    marginal = paste0(
      "likelihood(distribution = \"student_t\", mean = x, sd = ",
      sd, ", df = ", df, ")"
    ),
    observation = params$mean,
    desc = desc,
    dist_type = "continuous",
    plot = list(
      range = range,
      labs = list(x = "\u03F4", y = "L(\u03F4|x)")
    )
  )
}


noncentral_d_likelihood <- function(d, df) {
  if (df == 0) {
    stop("You must specify a `df` a non-central likelihood",
      call. = FALSE
    )
  }

  params <- list(d = d, df = df)
  desc <- paste0(
    "Object of class likelihood\n",
    "Distribution family: non-central t (d scaled)\n\n",
    "Parameters\n",
    "d: ", params$d, "\n",
    "DF:", params$df
  )

  # calculate the plot defaults
  variance <- d_variance(d, df)
  sd <- sqrt(variance)
  min <- d - 4 * sd
  max <- d + 4 * sd
  range <- c(min, max)
  func <- make_distribution(
    "non_central_t_dist",
    list(df = df, d = d)
  )


  new(
    Class = "likelihood",
    data = list(
      family = "noncentral_d",
      parameters = as.data.frame(params),
      fun = func
    ),
    func = func,
    marginal = paste0(
      "likelihood(distribution = \"noncentral_d\", d = x, df = ",
      df, ")"
    ),
    observation = params$d,
    desc = desc, dist_type = "continuous",
    plot = list(
      range = range,
      labs = list(x = "\u03F4", y = "L(\u03F4|x)")
    )
  )
}

noncentral_t_likelihood <- function(t, df) {
  if (df == 0) {
    stop("You must specify a `df` a non-central likelihood",
      call. = FALSE
    )
  }

  params <- list(t = t, df = df)
  desc <- paste0(
    "Object of class likelihood\n",
    "Distribution family: non-central t (t scaled)\n\n",
    "Parameters\n",
    "d: ", params$t, "\n",
    "DF:", params$df
  )

  d <- t * sqrt(df + 1)
  variance <- d_variance(d, df)
  sd <- sqrt(variance)
  min <- t - 4 * sd
  max <- t + 4 * sd
  # calculate the plot defaults
  range <- c(min, max)
  func <- make_distribution(
    "non_central_t_dist_t",
    list(df = df, t = t)
  )


  new(
    Class = "likelihood",
    data = list(
      family = "noncentral_t",
      parameters = as.data.frame(params),
      fun = func
    ),
    func = func,
    marginal = paste0(
      "likelihood(distribution = \"noncentral_t\", t = x, df = ",
      df, ")"
    ),
    observation = params$t,
    desc = desc,
    dist_type = "continuous",
    plot = list(
      range = range,
      labs = list(x = "\u03F4", y = "L(\u03F4|x)")
    )
  )
}


binomial_likelihood <- function(successes, trials) {
  params <- list(successes = successes, trials = trials)
  desc <- paste0(
    "Object of class likelihood\n",
    "Distribution family: binomial\n\n",
    "Parameters\n",
    "successes: ", params$successes, "\n",
    "trials: ", params$trials
  )

  # calculate the plot defaults
  range <- c(0, 1)
  func <- make_distribution(
    "binom_dist",
    list(successes = successes, trials = trials)
  )


  new(
    Class = "likelihood",
    data = list(
      family = "binomial",
      parameters = as.data.frame(params),
      fun = func
    ),
    func = func,
    marginal = paste0(
      "likelihood(distribution = \"binomial\", trials = ",
      trials, ", successes =  x)"
    ),
    observation = params$successes,
    desc = desc,
    dist_type = "discrete",
    plot = list(
      range = range,
      labs = list(x = "\u03F4", y = "L(\u03F4|x)")
    )
  )
}
