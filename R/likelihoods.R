


dt.scaled <- function (x, df, mean = 0, sd = 1, ncp, log = FALSE)
{
  if (!log)
    stats::dt((x - mean)/sd, df, ncp = ncp, log = FALSE)/sd
  else stats::dt((x - mean)/sd, df, ncp = ncp, log = TRUE) -
    log(sd)
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
#' * \code{student_t} a scaled and shifted or non-central t-distribution
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
#' ### scaled and shifted t-distribution
#' * \code{mean} mean of the scaled and shifted t likelihood
#' * \code{sd} standard deviation of the scaled and shifted t likelihood
#' * \code{df} degrees of freedom
#'
#' ### non-central t-distribution
#' * \code{ncp} The non-centrality parameter
#' * \code{df} The degrees of freedom
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
#' # specify non-central t likelihood (no currently implemented)
#' # likelihood(distribution = "student_t", ncp = 10, df = 10)
likelihood <- function(distribution, ...) {
  parameters <- as.list(match.call(expand.dots = TRUE))

  distribution <- paste0(parameters$distribution %||%
    "normal", "_likelihood")



  lik_fun <- purrr::partial(.f = rlang::as_function(distribution), ...)

  return(lik_fun())
}


# function that specifies a normal likelihood
normal_likelihood <- function(mean, sd) { # nolint
  if (missing(mean) | missing(sd)) {
    stop("You must specify a `mean` and `se` for a normal likelihood",
      call. = FALSE
    )
  }


  params <- list(mean = mean, sd = sd)
  desc <- paste0(
    "Parameters\nMean: ", params[[1]],
    "\nSD: ", params[[2]]
  )

  # calculate the plot defaults
  width <- 4 * sd
  range <- c(mean - width, mean + width)

  new(
    Class = "likelihood",
    data = list(
      likelihood_type = "normal", parameters = params,
      observation = mean
    ),
    func = eval(parse(
      text =
        (paste0(
          "function(theta) dnorm(x = theta, mean = ",
          mean, ", sd = ",
          sd, ")"
        ))
    )),
    marginal = paste0(
      "likelihood(distribution = \"normal\", mean = x, sd = ",
      sd, ")"
    ),
    desc = desc,
    dist_type = "continuous",
    plot = list(
      range = range,
      labs = list(x = "\u03F4", y = "L(\u03F4|x)")
    )
  )
}


# function that specifies a student_t likelihood
student_t_likelihood <- function(...) {
  parameters <- as.list(match.call(expand.dots = TRUE))


  # TODO: check parameter names to differentiate between non-central t and scaled and shifted t-distribution


  mean <- parameters$mean
  sd <- parameters$sd
  df <- parameters$df

  params <- list(mean = mean, sd = sd, df = df)
  desc <- paste0(
    "Parameters\nMean: ", params[[1]],
    "\nSD: ", params[[2]],
    "\ndf: ", params[[3]]
  )

  # calculate the plot defaults
  width <- 4 * sd
  range <- c(mean - width, mean + width)

  new(
    Class = "likelihood",
    data = list(
      likelihood_type = "student_t",
      parameters = params, observation = mean
    ),
    func = eval(parse(
      text =
        (paste0(
          "function(theta) dt.scaled(x = theta, df = ",
          df, ", mean = ", mean, ", sd = ", sd, ")"
        ))
    )),
    marginal = paste0(
      "likelihood(distribution = \"normal\", mean = x, sd = ",
      sd, ")"
    ),
    desc = desc,
    dist_type = "continuous",
    plot = list(
      range = range,
      labs = list(x = "\u03F4", y = "L(\u03F4|x)")
    )
  )
}
