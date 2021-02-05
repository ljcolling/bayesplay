
#################################################################
##                 DEFINITIONS OF THE PRIORS                   ##
#################################################################

# class definition


#' Specify a prior
#'
#' @param distribution the prior distribution (see details)
#' @param ... see details
#'
#' @details
#' ## Available distributions
#' The following distributions can be used for the prior
#' * \code{normal} a normal distribution
#' * \code{uniform} a uniform distribution
#' * \code{point} a point
#' The parameters that need to be specified will be dependent on the
#' distribution
#' ## normal distribution
#' When \code{distribution} is set to \code{normal} then the following
#' parameters must be set
#' * \code{mean} mean of the normal prior
#' * \code{sd} standard deviation of the normal prior
#' * \code{range} (optional) a vector specifying a paramter range
#' ## uniform distribution
#' When \code{distribution} is set to \code{uniform} then the following
#' parameters must be set
#' * \code{min} the lower bound
#' * \code{max} the upper bound
#' ## point
#' When \code{distribution} is set to \code{point} then the following
#' parameters may be set
#' * \code{point} the location of the point prior (default: 0)
#' @md
#' @return an object of class \code{prior}
#' @export
#'
#' @examples
#' # specify a half-normal (range 0 to Infinity) prior
#' prior(distribution = "normal", mean = 0, sd = 13.3, range = c(0, Inf))
#'
#' # specify a normal prior
#' prior(distribution = "normal", mean = 0, sd = 13.3)
#'
#' # specify a uniform prior
#' prior(distribution = "uniform", min = 0, max = 20)
#'
#' # specify a point prior
#' prior(distribution = "point", point = 0)
prior <- function(distribution, ...) {
  parameters <- as.list(match.call(expand.dots = TRUE))


  # set the default range of support
  if (parameters$distribution == "beta") {
    default_range <- c(0, 1)
  } else {
    default_range <- c(-Inf, Inf)
  }

  range <- parameters$range %||% default_range # nolint



  # prior function needs parameters for
  # distribution - normal, student_t, beta, cauchy, uniform, point
  # parameters - parameters for the distributions
  # range_of_support :: for one tailed etc

  distribution <- paste0(parameters$distribution %||%
    "uniform", "_prior")

  lik_fun <- purrr::partial(
    .f = rlang::as_function(distribution),
    range = range, ...
  )

  return(lik_fun())
}


# function that specifies a normal prior
# working on for ./R/plotting.R
normal_prior <- function(mean, sd, range) {
  if (missing(mean) | missing(sd)) {
    stop("You must specify `mean` and `sd` for a normal prior", call. = FALSE)
  }

  func <- make_distribution("norm_dist", list(mean = mean, sd = sd))
  # normalise the pior
  # get the normalising factor
  if (range[1] != range[2]) {
    k <- 1 / stats::integrate(
      f = func,
      lower = range[1],
      upper = range[2]
    )$value
  } else {
    # It's a point prior
    k <- 1
  }

  if (k != 1) {
    func <- make_distribution("half_norm",
                              list(range = range,
                                   mean = mean,
                                   sd = sd,
                                   k = k))
  }


  params <- list(mean = mean, sd = sd, range = range)

  new(
    Class = "prior",
    data = list(parameters = params, distribution = "normal",
                prior_function = Vectorize(func)),
    theta_range = range,
    type = "normal",
    func = Vectorize(func),
    desc = purrr::map(list(params),
                      function(x) paste0(names(x), " : ", x, "\n"))[[1]],
    dist_type = "continuous",
    plot = list(
      range = c(mean - qnorm(p = 0.9999) * sd, mean + qnorm(p = 0.9999) * sd),
      labs = list(x = "\u03F4", y = "P(\u03F4)")
    ),
    parameters = list(mean = mean, sd = sd),
    function_text = paste0("prior(\"normal\", mean = ", mean, ", sd =", sd, ")")
  )
}


# function that specifies a point prior
point_prior <- function(range, point = 0) {
  if (missing(point)) {
    warning("Point value is missing. Assuming 0", call. = FALSE)
  }
  width <- 4
  range <- c(point - width, point + width)
  params <- list(point = point)
  func <- make_distribution("point", list(point = point))
  new(
    Class = "prior",
    data = list(parameters = params, distribution = "point",
                prior_function = func),
    theta_range = c(point, point),
    func = func,
    type = "point",
    dist_type = "point",
    plot = list(
      range = range,
      labs = list(x = "\u03F4", y = "P(\u03F4)")
    ),
    parameters = list(point = point),
    function_text = paste0("prior(\"point\", point = ", point, ")"),
    desc = purrr::map(list(params),
                      function(x) paste0(names(x), " : ", x, "\n"))[[1]]
  )
}


# function that specifies a uniform prior
uniform_prior <- function(min, max, range) {
  if (missing(min) | missing(max)) {
    stop("You must specify `min` and `max` for a uniform  prior", call. = FALSE)
  }


  func <- make_distribution("uni_dist", list(min = min, max = max))
  params <- list(min = min, max = max)
  new(
    Class = "prior",
    data = list(parameters = params, distribution = "uniform",
                prior_function = func),
    theta_range = range,
    func = func,
    type = "normal",
    desc = purrr::map(list(params),
                      function(x) paste0(names(x), " : ", x, "\n"))[[1]],
    dist_type = "continuous",
    plot = list(
      range = c(min - abs(min - max), max + abs(min - max)),
      labs = list(x = "\u03F4", y = "P(\u03F4)")
    ),
    parameters = list(mean = mean, sd = sd),
    function_text = paste0(
      "prior(\"uniform\", min = ",
      min, ", max =", max, ")"
    )
  )
}

# function that specifies a student_t prior
student_t_prior <- function(mean, sd, df, range) {
  if (missing(mean) | missing(sd) | missing(df)) {
    stop("You must specify `mean`, `sd`, and `df` for a student_t prior",
      call. = FALSE
    )
  }


  func <- make_distribution("t_dist",
                            list(mean = mean, sd = sd, df = df, ncp = 0))
  # normalise the pior
  # get the normalising factor
  if (range[1] != range[2]) {
    k <- 1 / stats::integrate(
      f = func,
      lower = range[1],
      upper = range[2]
    )$value
  } else {
    # It's a point prior
    k <- 1
  }

  if (k != 1) {
    func <- make_distribution("half_t",
                              list(range = range,
                                   mean = mean,
                                   sd = sd,
                                   df = df,
                                   ncp = 0,
                                   k = k))
  }

  new(
    Class = "prior",
    data = list(mean = mean, sd = sd, df = df, distribution = "student_t",
                prior_function = Vectorize(func)),
    theta_range = range,
    func = Vectorize(func),
    type = "normal",
    desc = "",
    dist_type = "continuous",
    plot = list(
      range = c(mean - qnorm(p = 0.9999) * sd, mean + qnorm(p = 0.9999) * sd),
      labs = list(x = "\u03F4", y = "P(\u03F4)")
    ),
    parameters = list(mean = mean, sd = sd),
    function_text = paste0("prior(\"normal\", mean = ", mean, ", sd =", sd, ")")
  )
}

cauchy_prior <- function(location = 0, scale, range) {
  func <- make_distribution("cauchy_dist",
                            list(location = location, scale = scale))
  # normalise the pior
  # get the normalising factor
  if (range[1] != range[2]) {
    k <- 1 / stats::integrate(
      f = func,
      lower = range[1],
      upper = range[2]
    )$value
  } else {
    # It's a point prior
    k <- 1
  }

  if (k != 1) {
    func <- make_distribution("half_cauchy",
                              list(range = range,
                                   location = location,
                                   scale = scale,
                                   k = k))
  }

  new(
    Class = "prior",
    data = list(location = location,
                scale = scale,
                distribution = "cauchy",
                normalizing_constant = k,
                prior_function = Vectorize(func)),
    theta_range = range,
    func = Vectorize(func),
    type = "normal",
    desc = "",
    dist_type = "continuous",
    plot = list(
      range = c(location - qnorm(p = 0.9999) * scale,
                location + qnorm(p = 0.9999) * scale),
      labs = list(x = "\u03F4", y = "P(\u03F4)")
    ),
    parameters = list(location = location, scale = scale),
    function_text = paste0("prior(\"cauchy\", location = ",
                           location, ", scale =",
                           scale, ")")
  )
}
