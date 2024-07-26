data(dengue2020)
data_limpia <- limpiar_data_sivigila(dengue2020)

test_that("`fecha_inisintomas` funciona correctamente", {
  data_agrupada <- agrupar_fecha_inisintomas(data_event = data_limpia)

  expect_s3_class(data_agrupada, "data.frame")
  expect_true("cod_eve" %in% names(data_agrupada))
  expect_true("nombre_evento" %in% names(data_agrupada))
  expect_true("ano" %in% names(data_agrupada))
  expect_true("ini_sin" %in% names(data_agrupada))
  expect_true("semana" %in% names(data_agrupada))
  expect_true("casos" %in% names(data_agrupada))

  expect_equal(
    data_agrupada[["casos"]],
    c(1, 1, 1, 7, 2, 4, 6, 6, 3, 5, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
  )

  plot <- plot_fecha_inisintomas(data_agrupada = data_agrupada)
  expect_s3_class(plot, "ggplot")
})

test_that("`fecha_inisintomas` maneja errores correctamente", {
  expect_error(
    agrupar_fecha_inisintomas(data_event = list(a = 1, b = 2)),
    "El parametro data_event debe ser un data.frame"
  )

  expect_error(
    agrupar_fecha_inisintomas(
      data_event = data_limpia,
      col_fecha = TRUE
    ),
    "El parametro col_fecha debe ser una cadena de caracteres"
  )

  expect_error(
    plot_fecha_inisintomas(data_agrupada = list(a = 1, b = 2)),
    "El parametro data_agrupada debe ser un data.frame"
  )

  expect_error(
    plot_fecha_inisintomas(
      data_agrupada =
        data.frame(semana = 1, casos = 1),
      col_fecha = 1
    ),
    "El parametro col_fecha debe ser una cadena de caracteres"
  )
})
