
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


  # binomial distribution
  binom_dist <- g("function({params$x})",
                  " dbinom(x = {params$successes},",
                  " size = {params$trials},",
                  " prob = {params$x})")

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


  # beta distribution
  beta_dist <- g("function({params$x})",
                 " dbeta(x = {params$x},",
                 " shape1 = {params$alpha},",
                 " shape2 = {params$beta})")

  return_func <- eval(parse(text = dist_name))
  return(eval2(return_func))
}



#' @importFrom methods new
#' @importFrom stats qnorm sd integrate


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
  # marginal <- function(theta) {
      # likelihood_func(theta = theta) * (prior_func(theta = theta))
    # }

  marginal <- calc_marginal(likelihood, prior)

  if (theta_range[1] != theta_range[2]) {
    alt_val <- suppressWarnings(stats::integrate(marginal,
                                                 theta_range[1],
                                                 theta_range[2])$value)
  } else {
    alt_val <- marginal(theta_range[[1]])
  }
  # alt_func <- marginal
  data <- list(
    integral = alt_val,
    marginal = marginal,
    posterior_function = posterior_mod,
    prediction_function = make_predict(likelihood, prior),
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
  if(class(obj) == "predictive"){
    return(new("auc", obj$integral))
  } else if (class(obj) == "likelihood") {
    return(stats::integrate(obj$fun, -Inf, Inf)$value)
  }
}

#' @export
`/.auc` <- function(e1, e2) {
  new("bf", unclass(e1) / unclass(e2))
}

bfsay <- function(bf) {
  bf <- unclass(bf)
  if(bf < 1){
    bf_base <- bf
    bf <- 1 / bf
  } else {
    bf_base <- bf
  }

  ev_level <- dplyr::case_when(bf == 1 ~ "No evidence",
                              bf > 1 & bf <= 3 ~ "Anecdotal evidence",
                              bf > 3 & bf <= 10 ~ "Moderate evidence",
                              bf > 10 & bf <= 30 ~ "Strong evidence",
                              bf > 30 & bf <= 100 ~ "Very strong evidence",
                              bf > 100 ~ "Extreme evidence")


 cat("Using the levels from  Wagenmakers et al (2017)\n")
 cat("A BF of ", round(bf_base, 4), " indicates:\n")
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

make_predict <- function(data_model, prior_model) {
  g <- glue::glue

  marginal <- data_model@marginal #nolint
  marginal_func <- eval(parse(text = g("function(x) {marginal}"))) #nolint

  predictive_func <- function(x) integral(marginal_func(x) * prior_model)

  return(Vectorize(predictive_func))

}


#' @export
calc_posterior <- function(likelihood, prior) {
  make_posterior <- function(likelihood, prior, theta) {

    k <- bayesplay::integral(likelihood * prior)

    prior_func <- prior@func
    likelihood_func <- likelihood@func

    (prior_func(theta) *
     likelihood_func(theta)) / k
  }

  purrr::partial(make_posterior,
                 likelihood = likelihood,
                 prior = prior)
}


#' @export
calc_marginal <- function(likelihood, prior) {
  make_marginal <- function(likelihood, prior, theta) {


    prior_func <- prior@func
    likelihood_func <- likelihood@func

    (prior_func(theta) *
     likelihood_func(theta)) 
  }

  purrr::partial(make_marginal,
                 likelihood = likelihood,
                 prior = prior)
}


#' @export
sd_ratio <- function(x, theta) {

    bf <- x@prior_obj$fun(theta) /
        x$posterior_function(theta)

    new("bf", bf)
}


# functions that need tidying 
#' @export
rnorm <- function(n, mean, sd) {
    as.numeric(mean + sd * scale(stats::rnorm(n)))
}

#' @export
rbvn <- function (n, m1, s1, m2, s2, rho) {
     X1 <- rnorm(n, m1, s1)
     X2 <- rnorm(n, m2 + (s2 / s1) * rho *
           (X1 - m1), sqrt((1 - rho^2) * s2^2))
     data.frame(V1 = X1, V2 = X2)
}

#' @export
se <- function(x, na.rm = TRUE) {

    sd <- stats::sd(x, na.rm = na.rm)
    n <- length(x)
    sd / sqrt(n)
}


#' @export
mean_ci <- function(x, group_var, measure_var) {

    g <- rlang::enexpr(group_var)
    m <- rlang::enexpr(measure_var)
    grouped <- dplyr::group_by(x, !!g)
    dplyr::summarise(grouped, mean = mean(!!m), ci = 1.96 * se(!!m))

}

#' @export
integral <- function(obj) {
  if(class(obj) == "predictive"){
    return(new("auc", obj$integral))
  } else if (class(obj) == "likelihood") {
    return(stats::integrate(obj$fun, -Inf, Inf)$value)
  }
}


#' @export
new_observation <- function(obj, newdata, type = "auc") {

  likelihood <- bayesplay::likelihood
  g <- glue::glue

  eval2 <- function(x) {
    eval(parse(text = x))
  }


  f <- eval2(g("function(x) {obj@likelihood_obj@marginal}"))

  if(type == "auc"){
    f2 <- function(x) {
        ret <- f(x) * obj@prior_obj
        ret$integral
    }
  }

  ret <- data.frame(x = newdata[,1], 
                    y = as.numeric(lapply(FUN = f2,
                                          X = as.numeric(newdata[,1]))))
  names(ret) <- c(names(newdata), "auc")
  return(ret)
}


#' @export
plot_posterior <- function(x) {

    ggplot2::ggplot() +
        ggplot2::geom_function(
        fun = x$posterior_function
        ) +
    ggplot2::xlim(x@prior_obj@plot$range) +
    ggplot2::labs(x = "\u03F4", y = "P(\u03F4|x)") +
    ggplot2::theme_minimal(base_size = 16) +
    NULL

}



#' @export
pp_plot <- function(x) {

    ggplot2::ggplot() +
        ggplot2::geom_function(
        fun = x$posterior_function,
        color = "blue"
        ) +
        ggplot2::geom_function(
        fun = x@prior_obj$fun,
        color = "red"
        ) +
    ggplot2::xlim(x@prior_obj@plot$range) +
    ggplot2::labs(x = "\u03F4", y = "P") +
    ggplot2::scale_colour_manual(c("red" = "prior", "blue" = "posterior")) +
    ggplot2::theme_minimal(base_size = 16) +
    NULL
}


