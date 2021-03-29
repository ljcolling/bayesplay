context("Plots")
test_that("Prior plots", {

  # Half normal prior
  half_normal <- prior("normal", 0, 45, range = c(0, Inf))
  half_normal_plot <- plot(half_normal)
  vdiffr::expect_doppelganger("half normal prior", half_normal_plot)




  normal <- prior("normal", 0, 45)
  normal_plot <- plot(normal)
  vdiffr::expect_doppelganger("normal prior", normal_plot)



  uniform <- prior("uniform", -10, 10)
  uniform_plot <- plot(uniform)
  vdiffr::expect_doppelganger("uniform prior", uniform_plot)

  point <- prior("point", 2)
  point_plot <- plot(point)
  vdiffr::expect_doppelganger("point prior", point_plot)
})
