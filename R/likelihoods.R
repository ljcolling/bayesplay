
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


d2_variance <- function(d, n1, n2) {
  (n1 + n2) / ((n1) * (n2)) + ((d * d) / (2 * (n1 + n2)))
}

likelihood_data_names <- c("family", "parameters", "likelihood_function")
#################################################################
##                 DEFINITIONS OF THE LIKELIHOODS              ##
#################################################################


#' Specify a likelihood
#'
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
#' \eqn{s_{pooled}}{s_pooled} is set as below:
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
likelihood <- function(family, ...) {
  if(missing(family)){
    stop("You must specify a `family`")
  }
  parameters <- as.list(match.call(expand.dots = TRUE))
  #parameters <- as.list(sys.call())
  family <- paste0(parameters$family %||%
    "normal", "_likelihood")

  if (family %in% c(
    "normal_likelihood",
    "student_t_likelihood",
    "binomial_likelihood",
    "noncentral_d2_likelihood",
    "noncentral_d_likelihood",
    "noncentral_t_likelihood"
  ) == FALSE) {
    stop(family, "is an Unkown likelihood function")
  }

  lik_fun <- purrr::partial(.f = rlang::as_function(family), ...)

  return(lik_fun())
}

likelihood_labs <- list(x = "theta", y = "Pr(Outcome)")


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

  data <- list(
    family = "normal",
    parameters = as.data.frame(params),
    fun = func
  )
  names(data) <- likelihood_data_names
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


  data <- list(
    family = "student_t",
    parameters = as.data.frame(params),
    fun = func
  )
  names(data) <- likelihood_data_names
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


noncentral_d_likelihood <- function(d, n) {
  if (n == 0) {
    stop("You must specify a `n` a non-central likelihood",
      call. = FALSE
    )
  }

  params <- list(d = d, n = n)
  desc <- paste0(
    "Object of class likelihood\n",
    "Distribution family: non-central t (d scaled)\n\n",
    "Parameters\n",
    "d: ", params$d, "\n",
    "N:", params$n
  )

  # calculate the plot defaults
  variance <- d_variance(d, n - 1)
  sd <- sqrt(variance)
  min <- d - 4 * sd
  max <- d + 4 * sd
  range <- c(min, max)
  func <- make_distribution(
    "non_central_d_dist",
    list(n = n, d = d)
  )


  data <- list(
    family = "noncentral_d",
    parameters = as.data.frame(params),
    fun = func
  )
  names(data) <- likelihood_data_names
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
    "non_central_t_dist",
    list(df = df, t = t)
  )


  data <- list(
    family = "noncentral_t",
    parameters = as.data.frame(params),
    fun = func
  )
  names(data) <- likelihood_data_names
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


  data <- list(
    family = "binomial",
    parameters = as.data.frame(params),
    fun = func
  )
  names(data) <- likelihood_data_names
  new(
    Class = "likelihood",
    func = func,
    data = data,
    marginal = paste0(
      "likelihood(family = \"binomial\", trials = ",
      trials, ", successes =  x)"
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

noncentral_d2_likelihood <- function(d, n1, n2) {
  if (n1 == 0 | n2 == 0) {
    stop("You must specify a `n1` and `n2` a non-central t likelihood",
      call. = FALSE
    )
  }

  params <- list(d = d, n1 = n1, n2 = n2)
  desc <- paste0(
    "Object of class likelihood\n",
    "Distribution family: non-central t (independent samples d scaled)\n\n",
    "Parameters\n",
    "d: ", params$d, "\n",
    "N1: ", params$n1, "\n",
    "N2: ", params$n2
  )

  # calculate the plot defaults
  variance <- d2_variance(d, n1,  n2)
  sd <- sqrt(variance)
  min <- d - 4 * sd
  max <- d + 4 * sd
  range <- c(min, max)
  func <- make_distribution(
    "non_central_d2_dist",
    list(n1 = n1, n2 = n2, d = d)
  )


  data <- list(
    family = "noncentral_d2",
    parameters = as.data.frame(params),
    fun = func
  )
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
}
