#' @export
plot.bayesplay <- function(x, n = 101, ...) {

  if(("dist_type" %in% slotNames(x)) == TRUE) {
    if (x@dist_type == "point") {
      data <- data.frame(x = x@parameters$point, y = 1)
      ggplot2::ggplot(data,
                      ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_point(size = 3, shape = 16) +
        ggplot2::geom_linerange(ggplot2::aes(x = x,
                                             y = NULL,
                                             ymax = y,
                                             ymin = 0)) +
        ggplot2::xlim(x@plot$range) +
        ggplot2::labs(x = x@plot$labs$x, y = x@plot$labs$y) +
        # ggplot2::theme_minimal(base_size = 16) +
        NULL

      } else if (x@dist_type == "continuous") {
      ggplot2::ggplot() +
        ggplot2::geom_function(
          fun = x@func,
          colour = "black",
          na.rm = TRUE,
          n = n
        ) +
        ggplot2::xlim(x@plot$range) +
        ggplot2::labs(x = x@plot$labs$x, y = x@plot$labs$y) + 
        ggplot2::theme_minimal(base_size = 16) +
        NULL

    } else if (x@dist_type == "discrete") {
      func <- x@func
      df <- data.frame(x = seq(0, x@data$parameters$trials) /
                     x@data$parameters$trials)
      df$y <- as.numeric(lapply(FUN = func, X = as.numeric(df[,1])))
      ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_point() +
        ggplot2::geom_line() +
        ggplot2::xlim(x@plot$range) +
        ggplot2::labs(x = x@plot$labs$x, y = x@plot$labs$y) +
        # ggplot2::theme_minimal(base_size = 12) +
        NULL
    }

    } else {
    # this needs to plot the marginal likelihood and 
    # not the prediction fuctions
    func <- x$marginal
    plot_range <- x@prior_obj@plot$range
    # func <- x$posterior_function
    ggplot2::ggplot() +
      ggplot2::geom_function(
        fun = func,
        colour = "black",
        na.rm = TRUE,
        n = n
      ) +
      ggplot2::labs(x = x@prior_obj@plot$labs$x, y = NULL) +
      ggplot2::xlim(plot_range) +
      # ggplot2::theme_minimal(base_size = 16) +
      NULL

  }
}



#' @export
visual_compare <- function(model1, model2, n = 101) {


    model1_name <- paste0(substitute(model1))
    model2_name <- paste0(substitute(model2))

    model1_func <- model1$prediction_function

    model2_func <- model2$prediction_function
    plot_range <- model1@likelihood_obj@plot$range

    likelihood_obj <- model1@likelihood_obj

   observation <- likelihood_obj@observation

    observation_df <- data.frame(x = c(observation, observation),
                                 y = c(model1_func(observation),
                                       model2_func(observation)),
                                 color = c(model1_name, model2_name))
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
      ggplot2::geom_point(data = observation_df,
      ggplot2::aes(x = x, y = y, color = color)) +
      ggplot2::scale_color_manual(values = c("red", "black"), name = "Models") +
      ggplot2::xlim(plot_range) +
      ggplot2::labs(x = "Observation", y = "Prediction") +
      ggplot2::theme_minimal(base_size = 16) +
      NULL

}


# THIS NEEDS TO BE INCORPORATED INTO A GENERAL PREDICT FUNCTION

#' @export
plot_predictions <- function(model, n = 101) {



    model_func <- model$prediction_function

    plot_range <- model@likelihood_obj@plot$range

    likelihood_obj <- model@likelihood_obj

   observation <- likelihood_obj@observation

    observation_df <- data.frame(x = observation,
                                 y = model_func(observation))

    ggplot2::ggplot() +
      ggplot2::geom_function(
        fun = model_func,
        na.rm = TRUE,
        n = n
      ) +
      ggplot2::geom_point(data = observation_df,
      ggplot2::aes(x = x, y = y)) +
      ggplot2::xlim(plot_range) +
      ggplot2::labs(x = "Observation", y = "Prediction") +
      ggplot2::theme_minimal(base_size = 16) +
      NULL

}
