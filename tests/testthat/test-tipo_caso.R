data(dengue2020)
data_limpia <- limpiar_data_sivigila(dengue2020)

test_that("`tipo_caso` funciona correctamente", {
  data_agrupada <- agrupar_tipo_caso(data_event = data_limpia)

  expect_s3_class(data_agrupada, "data.frame")
  expect_true("cod_eve" %in% names(data_agrupada))
  expect_true("nombre_evento" %in% names(data_agrupada))
  expect_true("ano" %in% names(data_agrupada))
  expect_true("tip_cas" %in% names(data_agrupada))
  expect_true("nombre_tip_cas" %in% names(data_agrupada))
  expect_true("casos" %in% names(data_agrupada))

  expect_equal(
    data_agrupada[["casos"]],
    c(36, 8, 1)
  )

  plot <- plot_tipo_caso(data_agrupada)
  expect_s3_class(plot, "ggplot")
})

test_that("`tipo_caso` maneja errores correctamente", {
  expect_error(
    agrupar_tipo_caso(data_event = list(a = 1, b = 2)),
    "El parametro data_event debe ser un data.frame"
  )

  expect_error(
    agrupar_tipo_caso(
      data_event = data_limpia,
      cols_tipo = TRUE
    ),
    "El parametro cols_tipo debe ser una cadena de caracteres"
  )

  expect_error(
    plot_tipo_caso(data_agrupada = list(a = 1, b = 2)),
    "El parametro data_agrupada debe ser un data.frame"
  )

  expect_error(
    plot_tipo_caso(data_agrupada = data_limpia, col_tipo = 1),
    "El parametro col_tipo debe ser una cadena de caracteres"
  )
})

test_that("`tipo_caso_years` funciona correctamente", {
  data_agrupada <- agrupar_tipo_caso(
    data_event = data_limpia,
    cols_tipo = c("tip_cas", "ano")
  )

  expect_s3_class(data_agrupada, "data.frame")
  expect_true("cod_eve" %in% names(data_agrupada))
  expect_true("nombre_evento" %in% names(data_agrupada))
  expect_true("ano" %in% names(data_agrupada))
  expect_true("tip_cas" %in% names(data_agrupada))
  expect_true("nombre_tip_cas" %in% names(data_agrupada))
  expect_true("casos" %in% names(data_agrupada))

  expect_equal(
    data_agrupada[["casos"]],
    c(36, 8, 1)
  )

  plot <- plot_tipo_caso_years(data_agrupada)
  expect_s3_class(plot, "ggplot")
})

test_that("`tipo_caso_years` maneja errores correctamente", {
  expect_error(
    plot_tipo_caso_years(data_agrupada = list(a = 1, b = 2)),
    "El parametro data_agrupada debe ser un data.frame"
  )

  expect_error(
    plot_tipo_caso_years(
      data_agrupada = data_limpia,
      col_tipo = 1
    ),
    "El parametro col_tipo debe ser una cadena de caracteres"
  )
  expect_error(
    plot_tipo_caso_years(
      data_agrupada = data_limpia,
      col_tipo = "tip_cas", col_year = 1
    ),
    "El parametro col_year debe ser una cadena de caracteres"
  )
})
