data(dengue2020)
data_limpia <- limpiar_data_sivigila(dengue2020)
data_limpia <- estandarizar_geo_cods(data_limpia)

test_that("`mpio` funciona correctamente", {
  data_agrupada <- agrupar_mpio(
    data_event = data_limpia,
    dpto = "Antioquia"
  )

  expect_s3_class(data_agrupada, "data.frame")
  expect_true("cod_eve" %in% names(data_agrupada))
  expect_true("nombre_evento" %in% names(data_agrupada))
  expect_true("ano" %in% names(data_agrupada))
  expect_true("cod_dpto_o" %in% names(data_agrupada))
  expect_true("departamento_ocurrencia" %in% names(data_agrupada))
  expect_true("cod_mun_o" %in% names(data_agrupada))
  expect_true("municipio_ocurrencia" %in% names(data_agrupada))
  expect_true("casos" %in% names(data_agrupada))

  expect_equal(
    data_agrupada[["casos"]],
    c(4, 1, 1, 1, 1, 1, 1, 1, 1)
  )

  plot <- plot_mpios(data_agrupada)
  expect_s3_class(plot, "ggplot")
})

test_that("`mpio` maneja errores correctamente", {
  expect_error(
    agrupar_mpio(data_event = list(a = 1, b = 2)),
    "El parametro data_event debe ser un data.frame"
  )

  expect_error(
    agrupar_mpio(data_event = data_limpia, col_mpio = TRUE),
    "El parametro col_mpio debe ser una cadena de caracteres"
  )

  expect_error(
    agrupar_mpio(
      data_event = data_limpia,
      porcentaje = "boolean"
    ),
    "El parametro porcentaje debe ser un booleano"
  )

  expect_error(
    plot_mpios(data_agrupada = list(a = 1, b = 2)),
    "El parametro data_agrupada debe ser un data.frame"
  )

  expect_error(
    plot_mpios(
      data_agrupada = data_limpia,
      col_mpios = 1
    ),
    "El parametro col_mpios debe ser una cadena de caracteres"
  )
})
