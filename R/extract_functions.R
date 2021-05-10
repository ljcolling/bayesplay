#' Extract posterior
#'
#' @export
extract_posterior <- function(x) {
  if (class(x) != "product") {
    stop("Object not of class product", call. = FALSE)
  }

  desc <- paste0(
    "Object of class posterior\n",
    "Likelihood:\n",
    sub(
      pattern = "Object of class likelihood\nDistribution family: ",
      x = x@likelihood_obj@desc, replacement = "Family: "
    ),
    "\n\nPrior:\n",
    sub(
      pattern = "Object of class prior\nDistribution family: ",
      x = x@prior_obj@desc, replacement = "Family: "
    ),
    "\n\nNormalising constant: ", round(x$integral, 4)
  )
  x@desc <- desc

  new(
    Class = "posterior",
    data = x@data,
    desc = x@desc,
    K = x@K,
    lik = x@lik,
    prior = x@prior,
    theta_range = x@theta_range,
    likelihood_obj = x@likelihood_obj,
    prior_obj = x@prior_obj
  )
}

#' Extract predictions
#' @export
extract_predictions <- function(x) {
  if (class(x) != "product") {
    stop("Object not of class product", call. = FALSE)
  }

  desc <- paste0(
    "Object of class marginal prediction\n",
    "Likelihood:\n",
    describe_likelihood(x = x@likelihood_obj, marginal = TRUE),
    "\n\nPrior:\n",
    sub(
      pattern = "Object of class prior\nDistribution family: ",
      x = x@prior_obj@desc, replacement = "Family: "
    ),
    "\n\nPrediction range: X = ", range_as_text(get_max_range(x)), "\n",
    "Current observation: X = ", x@likelihood_obj@observation
  )
  new(
    Class = "prediction",
    data = x@data,
    desc = desc,
    K = x@K,
    lik = x@lik,
    prior = x@prior,
    theta_range = x@theta_range,
    likelihood_obj = x@likelihood_obj,
    prior_obj = x@prior_obj
  )
}

# functions for describing likelihoods
describe_likelihood <- function(x = NULL, params = NULL, family = NULL, marginal = FALSE) { # nolint
  if (is.null(params) & is.null(family)) {
    params <- x$parameters
    family <- x$family
  }

  header <- "Object of class likelihood\n"
  if (marginal) {
    header <- ""
  }


  if (family == "normal") {
    return(paste0(
      header,
      "Distribution family: normal\n",
      "Parameters\n",
      "Mean: ", ifelse(marginal, "X", params$mean), "\n",
      "SD: ", params$sd
    ))
  }

  if (family == "student_t") {
    return(paste0(
      header,
      "Distribution family: student t\n",
      "Parameters\n",
      "Mean: ", ifelse(marginal, "X", params$mean), "\n",
      "SD: ", params$sd, "\n",
      "DF: ", params$df
    ))
  }

  if (family == "noncentral_d") {
    return(paste0(
      header,
      "Distribution family: non-central t (d scaled)\n",
      "Parameters\n",
      "d: ", ifelse(marginal, "X", params$d), "\n",
      "N: ", params$n
    ))
  }

  if (family == "noncentral_t") {
    return(paste0(
      header,
      "Distribution family: non-central t (t scaled)\n",
      "Parameters\n",
      "d: ", ifelse(marginal, "X", params$t), "\n",
      "DF: ", params$df
    ))
  }

  if (family == "noncentral_d2") {
    return(paste0(
      header,
      "Distribution family: non-central t (independent samples d scaled)\n",
      "Parameters\n",
      "d: ", ifelse(marginal, "X", params$d), "\n",
      "N1: ", params$n1, "\n",
      "N2: ", params$n2
    ))
  }

  if (family == "binomial") {
    return(paste0(
      header,
      "Distribution family: binomial\n",
      "Parameters\n",
      "successes: ", ifelse(marginal, "X", params$successes), "\n",
      "trials: ", params$trials
    ))
  }
}
