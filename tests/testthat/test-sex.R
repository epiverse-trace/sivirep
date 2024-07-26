data(dengue2020)
data_limpia <- limpiar_data_sivigila(dengue2020)

test_that("`sex` funciona correctamente", {
  data_agrupada <- agrupar_sex(
    data_event = data_limpia,
    porcentaje = TRUE
  )

  expect_s3_class(data_agrupada, "data.frame")
  expect_true("cod_eve" %in% names(data_agrupada))
  expect_true("nombre_evento" %in% names(data_agrupada))
  expect_true("ano" %in% names(data_agrupada))
  expect_true("sexo" %in% names(data_agrupada))
  expect_true("casos" %in% names(data_agrupada))

  expect_equal(
    data_agrupada[["casos"]],
    c(22, 25)
  )

  plot <- plot_sex(data_agrupada)
  expect_s3_class(plot, "ggplot")
})

test_that("`sex` maneja errores correctamente", {
  expect_error(
    agrupar_sex(data_event = list(a = 1, b = 2)),
    "El parametro data_event debe ser un data.frame"
  )

  expect_error(
    agrupar_sex(data_event = data_limpia, col_sex = TRUE),
    "El parametro col_sex debe ser una cadena de caracteres"
  )

  expect_error(
    agrupar_sex(
      data_event = data_limpia,
      porcentaje = "boolean"
    ),
    "El parametro porcentaje debe ser un booleano"
  )

  expect_error(
    plot_sex(data_agrupada = list(a = 1, b = 2)),
    "El parametro data_agrupada debe ser un data.frame"
  )

  expect_error(
    plot_sex(
      data_agrupada = data_limpia,
      col_sex = 1
    ),
    "El parametro col_sex debe ser una cadena de caracteres"
  )
})

test_that("`sex_semanaepi` funciona correctamente", {
  data_agrupada <- agrupar_sex_semanaepi(
    data_event = data_limpia,
    porcentaje = TRUE
  )

  expect_s3_class(data_agrupada, "data.frame")
  expect_true("cod_eve" %in% names(data_agrupada))
  expect_true("nombre_evento" %in% names(data_agrupada))
  expect_true("ano" %in% names(data_agrupada))
  expect_true("sexo" %in% names(data_agrupada))
  expect_true("semana" %in% names(data_agrupada))
  expect_true("casos" %in% names(data_agrupada))
  expect_true("porcentaje" %in% names(data_agrupada))

  expect_equal(
    data_agrupada[["casos"]],
    c(1, 15, 1, 1, 1, 1, 1, 1, 1, 1, 18, 1, 1, 1, 1, 1)
  )

  plot <- plot_sex_semanaepi(data_agrupada)
  expect_s3_class(plot, "ggplot")
})

test_that("`sex_semanaepi` maneja errores correctamente", {
  expect_error(
    agrupar_sex_semanaepi(data_event = list(a = 1, b = 2)),
    "El parametro data_event debe ser un data.frame"
  )

  expect_error(
    agrupar_sex_semanaepi(
      data_event = data_limpia,
      cols_sex = TRUE
    ),
    "El parametro cols_sex debe ser una cadena de caracteres"
  )

  expect_error(
    agrupar_sex_semanaepi(
      data_event = data_limpia,
      porcentaje = "boolean"
    ),
    "El parametro porcentaje debe ser un booleano"
  )

  expect_error(
    plot_sex_semanaepi(data_agrupada = list(a = 1, b = 2)),
    "El parametro data_agrupada debe ser un data.frame"
  )

  expect_error(
    plot_sex_semanaepi(
      data_agrupada = data_limpia,
      col_sex = 1
    ),
    "El parametro col_sex debe ser una cadena de caracteres"
  )

  expect_error(
    plot_sex_semanaepi(
      data_agrupada = data_limpia,
      col_semanaepi = 1
    ),
    "El parametro col_semanaepi debe ser una cadena de caracteres"
  )
})
