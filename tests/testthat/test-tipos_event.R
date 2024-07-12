data(dengue2020)
data_limpia <- limpiar_data_sivigila(dengue2020)

test_that("`tipos_event` funciona correctamente", {
  
  data_agrupada <- agrupar_eventos(data_event = data_limpia)
  expect_s3_class(data_agrupada, "data.frame")
  expect_true("cod_eve" %in% names(data_agrupada))
  expect_true("nombre_evento" %in% names(data_agrupada))
  expect_true("ano" %in% names(data_agrupada))
  expect_true("casos" %in% names(data_agrupada))
  
  expect_equal(data_agrupada[["casos"]], 45)
  
  plot <- plot_tabla_tipos_event(data_agrupada)
  expect_s3_class(plot, "kableExtra")
  
})

test_that("`tipos_event` maneja errores correctamente", {
  
  expect_error(agrupar_eventos(data_event = list(a = 1, b = 2)),
               "El parametro data_event debe ser un data.frame")
  
  expect_error(agrupar_eventos(data_event = data_limpia, col_event = TRUE),
               "El parametro col_event debe ser una cadena de caracteres")
  
  expect_error(plot_tabla_tipos_event(data_agrupada = list(a = 1, b = 2)),
               "El parametro data_agrupada debe ser un data.frame")
  
  expect_error(plot_tabla_tipos_event(data_agrupada = data_limpia,
                                      col_event = 1),
               "El parametro col_event debe ser una cadena de caracteres")
})
