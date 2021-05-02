
#' @export
plot.bayesplay <- function(x, n = 101, type = NULL, ...) {
  if (class(x) %in% c("likelihood", "prior")) {
    return(handle_prior_likelihood(x, n)) # TODO: have seperate plot_prior and
    # plot_likelihood
  }

  if (is.null(type)) {
    return(plot_weighted_likelihood(x, n))
  } else if (type == "posterior") {
    return(plot_posterior(x, n))
  } else if (type == "pp") {
    return(plot_pp(x, n))
  } else if (type == "prediction") {
    model_name <- paste0(substitute(x))
    return(plot_prediction(x, n, model_name))
  } else if (type == "updating") {

    return(NULL)

  }


}


# TODO: Split the plots into layers, so that the layes can be joined
# Also allow plot function to output layer only

plot_prediction <- function(x, n, model_name) {
  likelihood_obj <- x@likelihood_obj
  likelihood_family <- likelihood_obj$family

  # now call the functions for different families
  if (likelihood_family == "binomial") {
    return(handle_binomial_marginal(x, n, model_name))
  }

  return(handle_other_marginal(x, n, model_name))

}




handle_binomial_marginal <- function(x, n, model_name) {
  model_func <- x$prediction_function
  plot_range <- c(0, x@likelihood_obj$parameters$trials)
  observation <- x@likelihood_obj@observation

  observation_df <- data.frame(
    x = observation,
    y = model_func(observation),
    color = model_name
  )

  observation_range <- seq(plot_range[1], plot_range[2], 1)

  counterfactual <- data.frame(
    x = observation_range,
    y = unlist(lapply(observation_range, FUN = model_func))
  )


  ggplot2::ggplot() +
    ggplot2::geom_line(
      data = counterfactual,
      ggplot2::aes(x = x, y = y, colour = model_name)
    ) +
    ggplot2::geom_point(
      data = counterfactual,
      ggplot2::aes(x = x, y = y, colour = model_name),
      size = 2, shape = 21, fill = "white"
    ) +
    ggplot2::geom_point(
      data = observation_df,
      ggplot2::aes(x = x, y = y, colour = model_name),
      size = 2, shape = 16
    ) +
    ggplot2::labs(x = "Outcome", y = "Marginal probability") +
    ggplot2::scale_color_manual(
      values = "black",
      name = NULL,
      labels = NULL,
      guide = FALSE
    ) +
    ggplot2::scale_x_continuous(
      limits = plot_range,
      breaks = integer_breaks()
    ) +
    NULL
}

get_max_range <- function(x) {

  pr <- x@prior_obj@plot$range
  lk <- x@likelihood_obj@plot$range
  min_value <- min(c(pr, lk))
  max_value <- max(c(pr, lk))
  return(c(min_value, max_value))
}



handle_other_marginal <- function(x, n, model_name) {

  model_func <- x$prediction_function
  plot_range <- get_max_range(x)
  observation <- x@likelihood_obj@observation

  observation_df <- data.frame(
    x = observation,
    y = model_func(observation),
    color = model_name
  )


  ggplot2::ggplot() +
    ggplot2::geom_function(fun = model_func, 
                           ggplot2::aes(colour = model_name)) +
    ggplot2::geom_point(
      data = observation_df,
      ggplot2::aes(x = x, y = y, colour = model_name),
      size = 2, shape = 16,
    ) +
    ggplot2::labs(x = "Outcome", y = "Marginal probability") +
    ggplot2::scale_color_manual(
      values = "black",
      name = NULL,
      labels = NULL,
      guide = FALSE,
    ) +
    ggplot2::scale_x_continuous(
      limits = plot_range,
    ) +
    NULL


}


marginal_probability <- function(model1, n = 101) {
  plot_range1 <- model1@prior_obj@plot$range
  plot_range2 <- model1@likelihood_obj@plot$range
  plot_range <- c(
    min(plot_range1[1], plot_range2[1]),
    max(plot_range1[2], plot_range2[2])
  )


  if (likelihood_family == "binomial") {
    plot_range <- c(0, model1@likelihood_obj$parameters$trials)

    observation <- likelihood_obj@observation



    observation_range <- seq(plot_range[1], plot_range[2], 1)

    counterfactual <- data.frame(
      x = observation_range,
      y = unlist(lapply(observation_range, FUN = model1_func))
    )
  }




  ggplot2::ggplot() +
    ggplot2::geom_point(
      data = counterfactual,
      ggplot2::aes(x = x, y = y),
      size = 2, shape = 21, fill = "white"
    ) +
    ggplot2::geom_point(
      data = observation_df,
      ggplot2::aes(x = x, y = y),
      size = 2, shape = 16
    ) +
    xlim(plot_range) +
    labs(x = "Outcome", y = "Marginal probability")
}

handle_prior_likelihood <- function(x, n) {
  if (x@dist_type == "point") {
    return(plot_point(x, n))
  } else if (x@dist_type == "continuous") {
    return(plot_continuous(x, n))
  } else if (x@dist_type == "discrete") {
    return(plot_discrete(x, n))
  }
}

plot_point <- function(x, n) {
  data <- data.frame(x = x@parameters$point, y = 1)
  return(ggplot2::ggplot(
    data,
    ggplot2::aes(x = x, y = y)
  ) +
    ggplot2::geom_point(size = 3, shape = 16) +
    ggplot2::geom_linerange(ggplot2::aes(
      x = x,
      y = NULL,
      ymax = y,
      ymin = 0
    )) +
    ggplot2::xlim(x@plot$range) +
    ggplot2::labs(x = x@plot$labs$x, y = x@plot$labs$y) +
    # ggplot2::theme_minimal(base_size = 16) +
    NULL)
}

plot_continuous <- function(x, n) {
  return(ggplot2::ggplot() +
    ggplot2::geom_function(
      fun = x@func,
      colour = "black",
      na.rm = TRUE,
      n = n
    ) +
    ggplot2::xlim(x@plot$range) +
    ggplot2::labs(x = x@plot$labs$x, y = x@plot$labs$y) +
    # ggplot2::theme_minimal(base_size = 16) +
    NULL)
}

plot_discrete <- function(x, n) {
  func <- x@func
  df <- data.frame(x = seq(0, x@data$parameters$trials) /
    x@data$parameters$trials)
  df$y <- as.numeric(lapply(FUN = func, X = as.numeric(df[, 1])))
  return(ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::xlim(x@plot$range) +
    ggplot2::labs(x = x@plot$labs$x, y = x@plot$labs$y) +
    # ggplot2::theme_minimal(base_size = 12) +
    NULL)
}

plot_weighted_likelihood <- function(x, n) {
  func <- x$weighted_likelihood_function
  # func <- x$conditional_function
  plot_range <- x@prior_obj@plot$range

  if (x@likelihood_obj$family == "binomial") {
  
    plot_range <- c(0, 1)
  
  }

  return(ggplot2::ggplot() +
    ggplot2::geom_function(
      fun = func,
      colour = "black",
      na.rm = TRUE,
      n = n
    ) +
    ggplot2::labs(x = x@prior_obj@plot$labs$x, 
                  y = glue::glue("Pr(Outcome) × Pr({theta})")) +
    ggplot2::xlim(plot_range) +
    NULL)
}



plot_posterior <- function(x, n) {
  ggplot2::ggplot() +
    ggplot2::geom_function(
      fun = x$posterior_function,
      n = n
    ) +
    ggplot2::xlim(x@prior_obj@plot$range) +
    ggplot2::labs(x = posterior_labs$x, y = posterior_labs$y) + 
    NULL
}

plot_pp <- function(x, n) {
  ggplot2::ggplot() +
    ggplot2::geom_function(
      fun = x$posterior_function,
      ggplot2::aes(color = "blue")
    ) +
    ggplot2::geom_function(
      fun = x@prior_obj@func,
      ggplot2::aes(color = "red")
    ) +
    ggplot2::xlim(x@prior_obj@plot$range) +
    ggplot2::labs(x = posterior_labs$x, y = posterior_labs$y) + 
    ggplot2::scale_colour_manual(
      values = c("blue", "red"),
      labels = c("posterior", "prior"),
      name = NULL
    ) +
    # ggplot2::theme_minimal(base_size = 16) +
    NULL
}

#' Visually compare two models
#'
#' @param model1 a \code{predictive} object
#' @param model2 a \code{predictive} object
#'
#' @return A \code{ggplot2} object
#'
#' @examples
#' # define two models
#' data_model <- likelihood(distribution = "noncentral_d", d, 79)
#' h0_mod <- prior(distribution = "point", point = 0)
#' h1_mod <- prior(distribution = "normal", mean = 0, sd = 1)
#' m0 <- data_model * h0_mod
#' m1 <- data_model * h1_mod
#'
#' # visually compare the model
#' visual_compare(m0, m01)
#' @export
visual_compare <- function(model1, model2, n = 101, type = "compare") {

  model1_layer <- plot(model1, type = "prediction")
  model2_layer <- plot(model2, type = "prediction")
  # fix the naming  here
  if (type == "compare") {
    # replace the `visual_compare()` function with this
    return(suppressMessages(
      gginnards::append_layers(
        model1_layer,
        model2_layer$layers
      ) + ggplot2::scale_colour_manual(values = c("red", "blue"), name = "Model")
    ))
  }

  if (type == "ratio") {

    ratio_function <- function(x) {
      model1$prediction_function(x) / model2$prediction_function(x)
  }
 


    df <- data.frame(x = seq(0, 10, 1))
    df$y <- ratio_function(df$x)

    # ggplot2::ggplot(data = df) +
    ggplot2::ggplot() +
      ggplot2::geom_function(fun = ratio_function) + 
      # ggplot2::geom_line(ggplot2::aes(x = x, y = y)) +
      # ggplot2::geom_point(ggplot2::aes(x = x, y = y)) +
      ggplot2::xlim(c(-2, 2)) 
      # ggplot2::xlim(c(0, 10)) 


  }
}

handle_con_ratio <- function(likelihood_obj,
                             model1_func, model2_func,
                             model1_name, model2_name,
                             plot_range, observation_df,
                             difference_df,
                             n) {
  difference_func <- function(x) model1_func(x) / model2_func(x)

  ggplot2::ggplot() +
    ggplot2::geom_function(
      fun = difference_func,
      na.rm = TRUE,
      n = n
    ) +
    ggplot2::geom_point(
      data = difference_df,
      ggplot2::aes(x = x, y = y)
    ) +
    ggplot2::xlim(plot_range) +
    ggplot2::labs(
      x = "Observation",
      y = paste0(model1_name, " / ", model2_name)
    ) +
    # ggplot2::theme_minimal(base_size = 16) +
    ggplot2::geom_hline(yintercept = 1, linetype = 2) +
    NULL
}


handle_desc_ratio <- function(likelihood_obj,
                              model1_func, model2_func,
                              model1_name, model2_name,
                              plot_range, observation_df,
                              difference_df,
                              n) {
  max_value <- likelihood_obj$parameters$trials
  min_value <- 0

  difference_func <- function(x) model1_func(x) / model2_func(x)

  df <- data.frame(x = seq(
    0,
    likelihood_obj@data$parameters$trials
  ))

  difference_data <- df

  difference_data$y <- as.numeric(lapply(
    FUN = difference_func,
    X = as.numeric(df[, 1])
  ))

  integer_breaks <- function(n = 5, ...) {
    fxn <- function(x) {
      breaks <- floor(pretty(x, n, ...))
      names(breaks) <- attr(breaks, "labels")
      breaks
    }
    return(fxn)
  }

  ggplot2::ggplot() +
    ggplot2::geom_line(
      data = difference_data, ggplot2::aes(x = x, y = y),
      na.rm = TRUE
    ) +
    ggplot2::geom_point(
      data = difference_data,
      ggplot2::aes(x = x, y = y),
      size = 2, shape = 21, fill = "white"
    ) +
    ggplot2::geom_point(
      data = difference_df,
      ggplot2::aes(x = x, y = y)
    ) +
    ggplot2::labs(
      x = "Observation",
      y = paste0(model1_name, " / ", model2_name)
    ) +
    # ggplot2::theme_minimal(base_size = 16) +
    ggplot2::geom_hline(yintercept = 1, linetype = 2) +
    ggplot2::scale_y_log10() +
    ggplot2::scale_x_continuous(
      limits =
        c(min_value, max_value),
      breaks = integer_breaks()
    ) +
    NULL
}

handle_con_vc <- function(model1_func,
                          model2_func,
                          model1_name,
                          model2_name,
                          plot_range,
                          observation_df,
                          n) {
  ggplot2::ggplot() +
    ggplot2::geom_function(
      fun = model1_func,
      ggplot2::aes(colour = model1_name),
      na.rm = TRUE,
      n = n
    ) +
    ggplot2::geom_function(
      fun = model2_func,
      ggplot2::aes(colour = model2_name),
      na.rm = TRUE,
      n = n
    ) +
    ggplot2::geom_point(
      data = observation_df,
      ggplot2::aes(x = x, y = y, color = color)
    ) +
    ggplot2::scale_color_manual(values = c("red", "black"), name = "Models") +
    ggplot2::xlim(plot_range) +
    ggplot2::labs(x = "Observation", y = "Prediction") +
    # ggplot2::theme_minimal(base_size = 16) +
    NULL
}


handle_desc_vc <- function(likelihood_obj,
                           model1_func, model2_func,
                           model1_name, model2_name,
                           plot_range, observation_df,
                           n) {
  max_value <- likelihood_obj$parameters$trials
  min_value <- 0

  df <- data.frame(x = seq(
    0,
    likelihood_obj@data$parameters$trials
  ))

  model1_data <- df
  model2_data <- df

  model1_data$y <- as.numeric(lapply(
    FUN = model1_func,
    X = as.numeric(df[, 1])
  ))
  model2_data$y <- as.numeric(lapply(
    FUN = model2_func,
    X = as.numeric(df[, 1])
  ))

  ggplot2::ggplot() +
    # ggplot2::geom_line(data = model1_plot, ggplot2::aes(x = x, y = y)) +
    # ggplot2::geom_point(data = model1_plot, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_line(
      data = model1_data,
      ggplot2::aes(x = x, y = y, color = model1_name)
    ) +
    ggplot2::geom_line(
      data = model2_data,
      ggplot2::aes(x = x, y = y, color = model2_name)
    ) +
    ggplot2::geom_point(
      data = model1_data,
      ggplot2::aes(x = x, y = y, color = model1_name),
      size = 2, shape = 21, fill = "white"
    ) +
    ggplot2::geom_point(
      data = model2_data,
      ggplot2::aes(x = x, y = y, color = model2_name),
      size = 2, shape = 21, fill = "white"
    ) +
    ggplot2::scale_color_manual(values = c("red", "black"), name = "Models") +
    ggplot2::geom_point(
      data = observation_df,
      ggplot2::aes(x = x, y = y, color = color),
    ) +
    ggplot2::xlim(min_value, max_value) +
    ggplot2::labs(x = "Observation", y = "Prediction") +
    # # ggplot2::theme_minimal(base_size = 16) +
    NULL
}


# THIS NEEDS TO BE INCORPORATED INTO A GENERAL PREDICT FUNCTION

# plot_predictions <- function(model, n = 101) {
#   model_func <- model$prediction_function
#
#   plot_range <- model@likelihood_obj@plot$range
#
#   likelihood_obj <- model@likelihood_obj
#
#   observation <- likelihood_obj@observation
#
#   observation_df <- data.frame(
#     x = observation,
#     y = model_func(observation)
#   )
#
#   ggplot2::ggplot() +
#     ggplot2::geom_function(
#       fun = model_func,
#       na.rm = TRUE,
#       n = n
#     ) +
#     ggplot2::geom_point(
#       data = observation_df,
#       ggplot2::aes(x = x, y = y)
#     ) +
#     ggplot2::xlim(plot_range) +
#     ggplot2::labs(x = "Observation", y = "Prediction") +
#     # ggplot2::theme_minimal(base_size = 16) +
#     NULL
# }
