 
#' @export
plot.bayesplay <- function(x) {
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
      ggplot2::labs(x = x@plot$labs$x, y = x@plot$labs$y)
    } else if (x@dist_type == "continuous") {
    ggplot2::ggplot() +
      ggplot2::geom_function(
        fun = x@func,
        colour = "black"
      ) +
      ggplot2::xlim(x@plot$range) +
      ggplot2::labs(x = x@plot$labs$x, y = x@plot$labs$y)
  } else if (x@dist_type == "discrete") {

  }
}