#' @export
plot.bayesplay <- function(x, n = 101, type = NULL, ...) {
  if (class(x) %in% c("likelihood", "prior")) {
    return(handle_prior_likelihood(x, n))
  }

  if (is.null(type)) {
    return(plot_weighted_likelihood(x, n))
  } else if (type == "posterior") {
    return(plot_posterior(x, n))
  } else if (type == "pp") {
    return(plot_pp(x, n))
  }
}


# TODO: Split the plots into layers, so that the layes can be joined
# Also allow plot function to output layer only


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
  func <- x$marginal
  plot_range <- x@prior_obj@plot$range
  return(ggplot2::ggplot() +
    ggplot2::geom_function(
      fun = func,
      colour = "black",
      na.rm = TRUE,
      n = n
    ) +
    ggplot2::labs(x = x@prior_obj@plot$labs$x, y = NULL) +
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
    ggplot2::labs(x = "\u03F4", y = "P") +
    NULL
}

plot_pp <- function(x, n) {
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
    # ggplot2::theme_minimal(base_size = 16) +
    NULL
}

#' @export
visual_compare <- function(model1, model2, n = 101, type = NULL) {
  model1_name <- paste0(substitute(model1))
  model2_name <- paste0(substitute(model2))

  model1_func <- model1$prediction_function

  model2_func <- model2$prediction_function

  plot_range1 <- model1@prior_obj@plot$range
  plot_range2 <- model2@prior_obj@plot$range
  plot_range3 <- model1@likelihood_obj@plot$range
  plot_range <- c(
    min(plot_range1[1], plot_range2[1], plot_range3[1]),
    max(plot_range1[2], plot_range2[2], plot_range3[2])
  )


  likelihood_obj <- model1@likelihood_obj

  observation <- likelihood_obj@observation

  observation_df <- data.frame(
    x = c(observation, observation),
    y = c(
      model1_func(observation),
      model2_func(observation)
    ),
    color = c(model1_name, model2_name)
  )

  difference_df <- data.frame(
    x = c(observation),
    y = c(model1_func(observation) /
      model2_func(observation))
  )

  if (is.null(type)) {
    if (likelihood_obj@dist_type == "discrete") {
      handle_desc_vc(likelihood_obj, model1_func, model2_func, model1_name, model2_name, plot_range, observation_df, n)
    } else {
      handle_con_vc(model1_func, model2_func, model1_name, model2_name, plot_range, observation_df, n)
    }
  } else if (type == "ratio") {
    if (likelihood_obj@dist_type == "discrete") {
      handle_desc_ratio(likelihood_obj, model1_func, model2_func, model1_name, model2_name, plot_range, observation_df, difference_df, n)
    } else {
      handle_con_ratio(likelihood_obj, model1_func, model2_func, model1_name, model2_name, plot_range, observation_df, difference_df, n)
    }
  } else {
    stop("Type ", type, " not supported")
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

  difference_data$y <- as.numeric(lapply(FUN = difference_func, X = as.numeric(df[, 1])))

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
    ggplot2::xlim(min_value, max_value) +
    ggplot2::labs(
      x = "Observation",
      y = paste0(model1_name, " / ", model2_name)
    ) +
    # ggplot2::theme_minimal(base_size = 16) +
    NULL
}

handle_con_vc <- function(model1_func, model2_func, model1_name, model2_name, plot_range, observation_df, n) {
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

  model1_data$y <- as.numeric(lapply(FUN = model1_func, X = as.numeric(df[, 1])))
  model2_data$y <- as.numeric(lapply(FUN = model2_func, X = as.numeric(df[, 1])))

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
