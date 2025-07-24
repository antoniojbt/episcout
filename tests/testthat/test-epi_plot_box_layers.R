library(ggplot2)

df_box <- data.frame(
  group = factor(rep(c("A", "B"), each = 5)),
  value = rnorm(10)
)

# 1. only var_y with notch TRUE

test_that("notch parameter is set", {
  p <- epi_plot_box(df_box, var_y = "value", notch = TRUE)
  expect_true(p$layers[[1]]$geom_params$notch)
})

# 2. var_x and var_y include expected layers and jitter params

test_that("layers include box components", {
  p <- epi_plot_box(df_box, var_x = "group", var_y = "value")
  layer_classes <- sapply(p$layers, function(x) class(x$geom)[1])
  expect_equal(
    layer_classes,
    c("GeomErrorbar", "GeomBoxplot", "GeomPoint", "GeomPoint")
  )
})


test_that("jitter parameters are applied", {
  p <- epi_plot_box(
    df_box,
    var_x = "group", var_y = "value",
    jitter_shape = 17, jitter_alpha = 0.6
  )
  jitter_layer <- p$layers[[3]]
  expect_equal(jitter_layer$aes_params$shape, 17)
  expect_equal(jitter_layer$aes_params$alpha, 0.6)
})
